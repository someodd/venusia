#!/usr/bin/env runghc
{-
  grpg — a tiny, stateful Gopherspace “overlay”:
    • Presence tracking (“who’s here”) per room
    • Lightweight chat (“squeaks”)
    • Proxy menu rewriter (browse any gopher target through Venusia)
    • Deterministic, room-scoped loot spawn + simple bag
    • Minimal PvP (STR/DEF/LUCK from looted items)
    • Room-aware /cmd verb execution

  ---------------------------------------------------------------------------
  QUICK START (CLI, easiest for testing)
  ---------------------------------------------------------------------------
    # Browse a remote gopher location through the proxy rewriter:
    grpg proxy <host[:port]><selector>

    # Run a verb in the context of a specific room (say/look/loot/attack/pher):
    grpg cmd <host[:port]><selector> <user> <command...>

    # Legacy single-arg forms (convenient with Venusia search boxes):
    grpg "<you>:pher host[:port]/selector"
    grpg "<you>:say message"
    grpg "<you>:look"

  ---------------------------------------------------------------------------
  VENUSIA GATEWAY SETUP (routes.toml)
  ---------------------------------------------------------------------------
    [[gateway]]
    selector = "/gateway/games/grpg/proxy/*"
    search   = false
    wildcard = true
    menu     = false
    command  = "/var/gopher/output/grpg"
    arguments = ["$wildcard"]

    [[gateway]]
    selector = "/gateway/games/grpg/look/*"
    search   = false
    wildcard = true
    menu     = false
    command  = "/var/gopher/output/grpg"
    arguments = ["$wildcard:look"]

    [[gateway]]
    selector  = "/gateway/games/grpg/cmd/*"
    wildcard  = true
    menu      = false
    search    = true   # user input after a space becomes the "verb ..."
    command   = "/var/gopher/output/grpg"
    arguments = ["$wildcard", "$search"]

  The “proxy” endpoint rewrites a remote menu so that clicks route back
  through grpg (keeping presence/chat/loot). The “cmd” endpoint executes
  verbs (say/look/loot/attack/pher) in the current room context.

  ---------------------------------------------------------------------------
  STORAGE LAYOUT (location-only keys; usernames never appear in path keys)
  ---------------------------------------------------------------------------
    /var/gopher/output/saves/grpg/
      rooms/<host>/<port>/<selector-as-dirs>/
        .lock/holder            # cooperative mkdir-based lock
        presence/<user>         # unix epoch of last heartbeat
        chat.log                # tail is shown to visitors
        loot/*.tr               # room loot (moved items + spawn)
        meta.grpg               # (reserved, not required)
      chars/<user>.sav          # last room visited (host/port/sel)
      chars/<user>/items/*.tr   # user’s bag (treasure metadata)

    • Loot files (*.tr) are tiny “ini-like” metadata:
        name=Lucky Packet
        str=1 | def=1 | luck=1
        src=room|player
    • Presence TTL controls who is considered “here”.

  ---------------------------------------------------------------------------
  VERBS INSIDE A ROOM
  ---------------------------------------------------------------------------
    say <msg>          – append to chat (squeaks)
    look               – refresh menu (spawn if first visit; touch presence)
    loot               – move all room loot into your bag
    bag                – list your bag contents
    attack <user>      – simple turn-based PvP using STR/DEF derived stats
    pher host[:port]/sel
                       – “teleport” (set last room and render there)

  ---------------------------------------------------------------------------
  BUILD/RUN NOTES
  ---------------------------------------------------------------------------
    • Requires: `curl` in PATH (used to fetch remote gopher menus/pages)
    • Avoids NumericUnderscores for older GHC compatibility
    • Concurrency: best-effort cooperative per-room lock using mkdir
      - Backoff + optional break after N microseconds
      - Lock only wraps critical sections that modify room state

  ---------------------------------------------------------------------------
  COMPATIBILITY / SECURITY
  ---------------------------------------------------------------------------
    • Inputs are sanitized (host, port, user, selector segments). Still,
      only deploy where you control the filesystem under saveRoot.
    • Loot/stat generation is deterministic per room key; no RNG required.
    • PvP requires both attacker and defender to be presently “in room”.
    • This is intentionally simple; don’t rely on it for auth/ACLs.

-}

-- base/system imports
import System.Environment   (getArgs)
import System.Exit          (die)
import System.IO            (withFile, IOMode(..), hPutStrLn, hSetBuffering, BufferMode(..), stdout)
import System.FilePath      ((</>))
import System.Directory
  ( createDirectory, createDirectoryIfMissing, doesFileExist, doesDirectoryExist
  , listDirectory, removeDirectory, removeFile, renameFile )
import System.Process       (readProcess)
import Control.Exception    (try, SomeException)
import Control.Monad        (when, unless, void)
import Control.Concurrent   (threadDelay)

-- data/time
import Data.Char (isAlphaNum, toLower, toUpper, isDigit, isHexDigit, isPrint, isSpace, isAlpha)
import Data.List (isPrefixOf, intercalate, dropWhileEnd, isInfixOf)
import Data.Time.Clock.POSIX (getPOSIXTime)

--------------------------------------------------------------------------------
-- Proxy / routes.toml related constants
--------------------------------------------------------------------------------

-- | Host/port the rewritten links should target (your Venusia instance).
proxyHost   :: String
proxyHost   = "gopher.someodd.zip"

proxyPort   :: String
proxyPort   = "70"

-- | Prefix for the proxying endpoint in your routes.toml.
proxyPrefix :: String
proxyPrefix = "/gateway/games/grpg/proxy/"

-- | Prefix for room-aware command execution (index-search verb input).
cmdPrefix :: String
cmdPrefix = "/gateway/games/grpg/cmd/"

--------------------------------------------------------------------------------
-- General configuration knobs
--------------------------------------------------------------------------------

-- | Root of grpg save state (rooms, chars, loot).
saveRoot :: FilePath
saveRoot = "/var/gopher/output/saves/grpg"

-- | Seconds after which a presence heartbeat is considered stale.
presenceTTL :: Int
presenceTTL = 120

-- | Number of recent chat lines (“squeaks”) to display.
chatTailN :: Int
chatTailN = 5

-- | Cooperative lock tuning (microseconds and retry counts).
lockBackoffMicros, lockRetries, lockBreakAfterMicros :: Int
lockBackoffMicros     = 20000
lockRetries           = 200
lockBreakAfterMicros  = 1000000

--------------------------------------------------------------------------------
-- Small, reusable helpers
--------------------------------------------------------------------------------

-- | Read human-friendly loot names for the current room’s floor pile.
roomLootNames :: String -> String -> String -> IO [String]
roomLootNames h p s = do
  let ldir = roomLootDir h p s
  ok <- doesDirectoryExist ldir
  if not ok then pure [] else do
    fs <- listDirectory ldir
    if null fs then pure [] else do
      metas <- mapM (\f -> safeReadFile (ldir </> f)) fs
      let nameOf t = headDef "unnamed" [drop 5 l | l <- lines t, "name=" `isPrefixOf` l]
      pure (map nameOf metas)

-- | Safe head with default.
headDef :: a -> [a] -> a
headDef d xs = case xs of { (y:_) -> y; _ -> d }

-- | Trim ASCII whitespace on both ends.
trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

-- | Ensure a selector has a leading slash.
ensureLeadingSlash :: String -> String
ensureLeadingSlash s = case s of { ""->"/"; ('/':_)->s; _ -> '/':s }

-- | Usernames are lowercase [a-z0-9_-]; sanitized for file paths.
sanitizeUser :: String -> String
sanitizeUser = map toLower . filter (\c -> isAlphaNum c || c=='_' || c=='-')

-- | Hosts are lowercase and limited to [a-z0-9.-].
sanitizeHost :: String -> String
sanitizeHost = map toLower . filter (\c -> isAlphaNum c || c=='.' || c=='-')

-- | Ports are digits; default to "70" if none.
sanitizePort :: String -> String
sanitizePort s = let p = filter isDigit s in if null p then "70" else p

-- | Convert a selector into a safe directory path (segment-by-segment).
selectorPath :: String -> FilePath
selectorPath sel0 =
  let sel   = dropWhile (=='/') sel0
      parts = splitOn '/' sel
  in foldl (</>) "" (map clean parts)
  where
    clean "" = "_"
    clean xs = map (\c -> if isAlphaNum c || c `elem` ".-_" then c else '_') xs

-- | Best-effort file read; returns "" on error (no throw).
safeReadFile :: FilePath -> IO String
safeReadFile fp = either (const "") id <$> (try (readFile fp) :: IO (Either SomeException String))

-- | Overwrite a file with a single line of text.
writeText :: FilePath -> String -> IO ()
writeText fp s = withFile fp WriteMode (\h -> hPutStrLn h s)

-- | Append a single line to a file (creates file if needed).
appendLine :: FilePath -> String -> IO ()
appendLine fp s = withFile fp AppendMode (\h -> hPutStrLn h s)

-- | Return the last N lines from a list.
tailLines :: Int -> [String] -> [String]
tailLines n xs = drop (length xs - min n (length xs)) xs

-- | Percent-decode a subset of URL-encoded strings (e.g., selectors).
percentDecode :: String -> String
percentDecode [] = []
percentDecode ('%':a:b:xs)
  | isHexDigit a && isHexDigit b = toEnum (hexVal a * 16 + hexVal b) : percentDecode xs
  | otherwise                    = '%' : a : percentDecode (b:xs)
  where
    hexVal c
      | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
      | c >= 'A' && c <= 'F' = 10 + fromEnum c - fromEnum 'A'
      | c >= 'a' && c <= 'f' = 10 + fromEnum c - fromEnum 'a'
      | otherwise = 0
percentDecode (x:xs) = x : percentDecode xs

-- | Split a string on a delimiter char (no regex; keeps empty segments).
splitOn :: Char -> String -> [String]
splitOn c s = go s "" where
  go [] acc = [reverse acc]
  go (x:xs) acc | x==c      = reverse acc : go xs ""
                | otherwise = go xs (x:acc)

--------------------------------------------------------------------------------
-- Parse helpers: URIs, wildcards, legacy forms
--------------------------------------------------------------------------------

-- | Parse "host[:port]/selector" (leading gopher:// optional).
parseHostPortSel :: String -> Either String (String,String,String)
parseHostPortSel uri0 =
  let u = dropWhile (==' ') uri0
      s = if "gopher://" `isPrefixOf` u then drop 9 u else u
      (hostPort, sel0) = break (=='/') s
      sel  = case sel0 of { ""->"/"; "/"->"/"; _->sel0 }
      (hostRaw, portRaw) = break (==':') hostPort
      host = sanitizeHost hostRaw
      port = case portRaw of { (':':rest)->sanitizePort rest; _->"70" }
  in if null host then Left "invalid target (missing host)"
     else Right (host, port, ensureLeadingSlash sel)

-- | Parse "host~port/selector" (accepted by legacy paths).
parseLocBlob :: String -> Maybe (String,String,String)
parseLocBlob s = case break (=='~') s of
  (h,'~':rest) -> let (p, sel0) = break (=='/') rest
                  in Just (sanitizeHost h, sanitizePort p, ensureLeadingSlash sel0)
  _            -> Nothing

-- | Strip anything up to and including the last "/proxy/" segment.
stripToAfterProxy :: String -> String
stripToAfterProxy = stripToAfter "/proxy/"

-- | Strip anything up to and including the last occurrence of @pat@.
stripToAfter :: String -> String -> String
stripToAfter pat s =
  case findLast pat s of
    Nothing -> s
    Just i  -> drop (i + length pat) s
  where
    findLast p xs = go 0 (-1)
      where
        plen = length p
        go idx lastHit
          | idx > length xs - plen = if lastHit >= 0 then Just lastHit else Nothing
          | take plen (drop idx xs) == p = go (idx+1) idx
          | otherwise                    = go (idx+1) lastHit

-- | Drop a leading "<user>:pher " prefix from a proxy-embedded command.
dropUserPherPrefix :: String -> String
dropUserPherPrefix s =
  let (u, rest0) = break (==':') s
  in if null rest0 then s
     else
       let user = sanitizeUser u
           r1   = drop 1 rest0
       in if not (null user) && ("pher " `isPrefixOf` r1)
            then dropWhile (==' ') (drop 5 r1)
            else s

-- | Parse "<user>:<host>:<port><selector>" (cmd wildcard).
parseCmdWildcard :: String -> Either String (String,String,String,String)
parseCmdWildcard w0 =
  let w = trim w0
      (uPart, rest1) = break (==':') w
  in if null rest1 then Left "cmd wildcard missing user separator ':'"
     else
       let user = sanitizeUser uPart
           rest = drop 1 rest1
       in case parseHostPortSel rest of
            Right (h,p,s) -> Right (user,h,p,s)
            Left e        -> Left ("cmd wildcard bad host/port/selector: " ++ e)

-- | Split legacy "<user>:<host>:<port>/<selector>[:search...]" into (wildcard, search).
splitLegacyCmdArg :: String -> Either String (String,String)
splitLegacyCmdArg arg =
  case parseCmdWildcardPrefix arg of
    Left e -> Left e
    Right (wl, rest) ->
      case rest of
        (':':search) -> Right (wl, search)
        _            -> Right (wl, "")
  where
    parseCmdWildcardPrefix :: String -> Either String (String,String)
    parseCmdWildcardPrefix s =
      let (u, r1) = break (==':') s
      in if null r1 then Left "cmd legacy arg missing user ':'"
         else
           let r2 = drop 1 r1
               (host, r3) = break (==':') r2
           in if null r3 then Left "cmd legacy arg missing host/port ':'"
              else
                let portAndSel = drop 1 r3
                    (portDigits, r4) = span isDigit portAndSel
                in if null portDigits then Left "cmd legacy arg missing port digits"
                   else case r4 of
                     ('/':_) ->
                       let (selPart, rem') = break (==':') r4
                           wildcard = u ++ ":" ++ host ++ ":" ++ portDigits ++ selPart
                       in Right (wildcard, rem')
                     _ -> Left "cmd legacy arg missing selector beginning with '/'"

--------------------------------------------------------------------------------
-- Storage helpers (location-only) + treasure helpers
--------------------------------------------------------------------------------

-- | Print the treasure in the player’s bag (names only).
listTreasure :: String -> IO ()
listTreasure you = do
  let idir = userItemsDir you
  ok <- doesDirectoryExist idir
  if not ok then putStrLn (iLine "[grpg] you carry nothing.")
  else do
    fs <- listDirectory idir
    if null fs then putStrLn (iLine "[grpg] you carry nothing.")
    else do
      metas <- mapM (\f -> safeReadFile (idir </> f)) fs
      let getName t = headDef "unnamed" [drop 5 l | l <- lines t, "name=" `isPrefixOf` l]
          names = map getName metas
      putStrLn (iLine ("[grpg] treasure (" ++ show (length names) ++ "):"))
      mapM_ (putStrLn . iLine . ("  - "++)) names

-- Directory/file layout constructors
roomDir :: String -> String -> String -> FilePath
roomDir h p s = saveRoot </> "rooms" </> sanitizeHost h </> sanitizePort p </> selectorPath s

presenceDir, chatFile, metaPath :: String -> String -> String -> FilePath
presenceDir h p s = roomDir h p s </> "presence"
chatFile    h p s = roomDir h p s </> "chat.log"
metaPath    h p s = roomDir h p s </> "meta.grpg"

-- | Room loot directory (floor pile).
roomLootDir :: String -> String -> String -> FilePath
roomLootDir h p s = roomDir h p s </> "loot"

-- | Player’s items directory.
userItemsDir :: String -> FilePath
userItemsDir who = saveRoot </> "chars" </> sanitizeUser who </> "items"

-- | Player save file path.
charPath :: String -> FilePath
charPath who = saveRoot </> "chars" </> (sanitizeUser who ++ ".sav")

-- | Ensure character record + items dir exist (idempotent).
ensureChar :: String -> IO ()
ensureChar who = do
  let fp = charPath who
  ex <- doesFileExist fp
  unless ex $ do
    createDirectoryIfMissing True (saveRoot </> "chars")
    createDirectoryIfMissing True (userItemsDir who)
    writeText fp $ unlines
      [ "name=" ++ sanitizeUser who
      , "last_host="
      , "last_port="
      , "last_sel="
      ]

-- | Update the “last room” pointers for a user.
setLastRoom :: String -> String -> String -> String -> IO ()
setLastRoom who h p s =
  writeText (charPath who) $ unlines
    [ "name=" ++ sanitizeUser who
    , "last_host=" ++ sanitizeHost h
    , "last_port=" ++ sanitizePort p
    , "last_sel="  ++ ensureLeadingSlash s
    ]

-- | Read the last room visited by a user.
getLastRoom :: String -> IO (Maybe (String,String,String))
getLastRoom who = do
  let fp = charPath who
  ex <- doesFileExist fp
  if not ex then pure Nothing else do
    t <- safeReadFile fp
    let grab k n = [drop n l | l <- lines t, take n l == k]
        h = headDef "" (grab "last_host=" 10)
        p = headDef "" (grab "last_port=" 10)
        s = headDef "" (grab "last_sel="   9)
    pure $ if null h || null p || null s then Nothing else Just (h,p,s)
  where headDef d xs = case xs of { (y:_)->y; _->d }

--------------------------------------------------------------------------------
-- Best-effort cooperative room locks
--------------------------------------------------------------------------------

lockPath :: String -> String -> String -> FilePath
lockPath h p s = roomDir h p s </> ".lock"

lockMarker :: FilePath -> FilePath
lockMarker lp = lp </> "holder"

-- | Try to acquire the lock once (mkdir). Write a timestamp marker on success.
acquireLockOnce :: FilePath -> IO Bool
acquireLockOnce lp = do
  e <- try (createDirectory lp) :: IO (Either SomeException ())
  case e of
    Right _ -> do now <- round <$> getPOSIXTime; writeText (lockMarker lp) (show (now :: Int)); pure True
    Left _  -> pure False

-- | Force-break a lock directory (best-effort) after a long wait.
forceBreakLock :: FilePath -> IO ()
forceBreakLock lp = do
  void (try (removeFile (lockMarker lp)) :: IO (Either SomeException ()))
  void (try (removeDirectory lp)         :: IO (Either SomeException ()))

-- | Run an action while holding the room lock.
withRoomLock :: String -> String -> String -> IO a -> IO a
withRoomLock h p s action = do
  let lp = lockPath h p s
  createDirectoryIfMissing True (roomDir h p s)
  ok <- tryAcquire lp 0 0
  if not ok then die "grpg: failed to acquire room lock" else do
    r <- action
    void (try (removeFile (lockMarker lp)) :: IO (Either SomeException ()))
    void (try (removeDirectory lp)         :: IO (Either SomeException ()))
    pure r
  where
    tryAcquire lp n elapsed
      | n >= lockRetries = pure False
      | otherwise = do
          got <- acquireLockOnce lp
          if got then pure True
                 else do
                   let elapsed' = elapsed + lockBackoffMicros
                   when (elapsed' >= lockBreakAfterMicros) (forceBreakLock lp)
                   threadDelay lockBackoffMicros
                   tryAcquire lp (n+1) elapsed'

--------------------------------------------------------------------------------
-- Presence + chat (“squeaks”)
--------------------------------------------------------------------------------

-- | Heartbeat a user’s presence into a room (under lock).
touchPresence :: String -> String -> String -> String -> IO ()
touchPresence who h p s = withRoomLock h p s $ do
  let pd = presenceDir h p s
  createDirectoryIfMissing True pd
  now <- round <$> getPOSIXTime
  writeText (pd </> sanitizeUser who) (show (now :: Int))

-- | Return currently-present users (filtered by presenceTTL).
listPresent :: String -> String -> String -> IO [String]
listPresent h p s = do
  let pd = presenceDir h p s
  ok <- doesDirectoryExist pd
  if not ok then pure [] else do
    now <- round <$> getPOSIXTime
    fs  <- listDirectory pd
    pairs <- mapM (\n -> do ts <- safeReadFile (pd </> n)
                            let t = case reads ts of ((z,_):_) -> z; _ -> 0 :: Int
                            pure (n, now - t <= presenceTTL)) fs
    pure [n | (n,f) <- pairs, f]

-- | Print the last N chat lines for the room (if any).
showChatTail :: String -> String -> String -> IO ()
showChatTail h p s = do
  let cf = chatFile h p s
  ex <- doesFileExist cf
  if not ex then pure () else do
    ls <- fmap lines (safeReadFile cf)
    let ts = tailLines chatTailN ls
    unless (null ts) $ mapM_ (putStrLn . iLine) ("\n-- squeaks --" : ts)

-- | Append a chat message and re-render the room.
doSay :: String -> String -> String -> String -> String -> IO ()
doSay who h p s msg = do
  withRoomLock h p s $ do
    createDirectoryIfMissing True (roomDir h p s)
    appendLine (chatFile h p s) (sanitizeUser who ++ ": " ++ take 300 msg)
    now <- round <$> getPOSIXTime
    writeText (presenceDir h p s </> sanitizeUser who) (show (now :: Int))
  renderMenu (sanitizeUser who) h p s

--------------------------------------------------------------------------------
-- Fetch via curl (gopher://host:port/selector)
--------------------------------------------------------------------------------

-- | Fetch a gopher resource using curl (silent, follows redirects).
fetchGopher :: String -> String -> String -> IO (Either String String)
fetchGopher host port sel = do
  let url = "gopher://" ++ host ++ ":" ++ port ++ sel
  e <- try (readProcess "curl" ["-s", "--location", url] "") :: IO (Either SomeException String)
  pure $ either (const (Left "fetch failed")) Right e

--------------------------------------------------------------------------------
-- Menu construction / rewriting
--------------------------------------------------------------------------------

-- | Build an informational “i” line for Gopher menus (safe for any text).
iLine :: String -> String
iLine txt = 'i' : map (\c->if isPrint c then c else ' ') txt ++ "\tf\tlocalhost\t1"

-- | Build a “search” line (type 7) pointing to our cmd endpoint for this user/room.
searchLine :: String -> String -> String -> String
searchLine label sel host =
  '7' : label ++ "\t" ++ sel ++ "\t" ++ host ++ "\t" ++ proxyPort

-- | Construct a proxy selector that re-enters grpg with embedded user/pher.
mkProxySel :: String -> String -> String -> Char -> String -> String
mkProxySel you h p c s =
  proxyPrefix ++ sanitizeUser you ++ ":pher "
              ++ sanitizeHost h ++ ":" ++ (sanitizePort p ++ "/" ++ [c] ++ s)

-- | Rewrite a single gopher line from remote menu → local proxying link.
--   Note: text files (type '0') are rewritten to '1' to enter proxied view.
rewriteItem :: String -> String -> String
rewriteItem you line = case line of
  []      -> iLine ""
  (t:rest) ->
    if t == 'i' then line
    else case splitTabs rest of
      (disp:sel:h:p:_) ->
        let newSel = mkProxySel you (trimCR h) (trimCR p) t (trimCR sel)
            newType = if t == '0' then '1' else t
        in [newType] ++ disp ++ "\t" ++ newSel ++ "\t" ++ proxyHost ++ "\t" ++ proxyPort
      _ -> iLine line
  where
    splitTabs = splitOn '\t'
    trimCR    = reverse . dropWhile (== '\r') . reverse

-- | Heuristic: does the fetched body “look like” a gopher menu?
seemsMenu :: [String] -> Bool
seemsMenu ls = any (\l -> case l of
                            (t:rest) | t `elem` "0123456789+TgIhsMdcUpP'=" ->
                              length (splitOn '\t' rest) >= 3
                            _ -> False) ls

--------------------------------------------------------------------------------
-- Deterministic loot + stats
--------------------------------------------------------------------------------

-- | Simple 32-bit-ish accumulator hash (no RNG dependency).
hash32 :: String -> Int
hash32 = foldl (\h c -> (h * 33 + fromEnum c) `mod` 2147483647) 5381

-- | Load dictionary words for loot naming (fallback to built-ins if missing).
loadDict :: IO [String]
loadDict = do
  let cands = ["/usr/share/dict/words","/usr/dict/words"]
  ms <- mapM (\p -> do ex <- doesFileExist p
                       if ex then fmap lines (safeReadFile p) else pure []) cands
  let raw = filter good (concat ms)
  pure (if null raw then fallback else raw)
  where
    good w = let v = filter isAlpha w in length v == length w && length w >= 3 && length w <= 12
    fallback = ["Gopher","Burrow","Trophy","Badge","Charm","Archive","Tunnel","Bit","Packet","Glyph","Badge","Talisman"]

-- | Capitalize a single word (ASCII).
capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = toUpper c : map toLower cs

-- | Pick which stat a room’s loot will grant (biased).
treasureStatKind :: String -> String
treasureStatKind rk =
  let t = abs (hash32 ("type:"++rk)) `mod` 100
  in if t < 60 then "str" else if t < 90 then "def" else "luck"

-- | Build a friendly loot name from two dictionary words + kind.
treasureName :: [String] -> String -> String
treasureName dict rk =
  let n  = max 1 (length dict)
      i1 = abs (hash32 ("n1:"++rk)) `mod` n
      i2 = abs (hash32 ("n2:"++rk)) `mod` n
      kind = case treasureStatKind rk of
               "str"  -> "Bodybuilding"
               "def"  -> "Sturdy"
               _      -> "Lucky"
  in kind ++ " " ++ capitalize (dict !! i1) ++ " " ++ capitalize (dict !! i2)

-- | Spawn exactly one treasure the first time a room is created.
spawnTreasureIfFirst :: String -> String -> String -> IO ()
spawnTreasureIfFirst h p s = do
  let rdir = roomDir h p s
  existed <- doesDirectoryExist rdir
  withRoomLock h p s $ do
    when (not existed) $ do
      createDirectoryIfMissing True rdir
      let ldir = roomLootDir h p s
      createDirectoryIfMissing True ldir
      dict <- loadDict
      let rk   = h ++ ":" ++ p ++ s
          nm   = treasureName dict rk
          kind = treasureStatKind rk
          iid  = take 12 (show (abs (hash32 ("id:"++rk))))
          fp   = ldir </> (iid ++ ".tr")
      ex <- doesFileExist fp
      unless ex $
        writeText fp (unlines ["name="++nm, kind++"=1", "src=room"])

-- | Sum the player’s STR/DEF/LUCK from items in their bag.
sumStats :: String -> IO (Int,Int,Int)
sumStats you = do
  let idir = userItemsDir you
  ok <- doesDirectoryExist idir
  if not ok then pure (0,0,0) else do
    fs <- listDirectory idir
    vals <- mapM (safeReadFile . (idir </>)) fs
    let kvs = concatMap (map parseKV . lines) vals
        get k = sum [v | (k',v) <- kvs, k'==k]
    pure (get "str", get "def", get "luck")
  where
    parseKV l = case break (=='=') l of
                  (k,'=':v) -> (k, readInt v)
                  _         -> ("",0)
    readInt v = case reads v of ((z,_):_) -> z; _ -> 0

-- | Move all loot in room → player bag (under lock); print result.
doLoot :: String -> String -> String -> String -> IO ()
doLoot you h p s = withRoomLock h p s $ do
  let ldir = roomLootDir h p s
  ok <- doesDirectoryExist ldir
  if not ok then putStrLn (iLine "[grpg] nothing to loot.")
  else do
    fs <- listDirectory ldir
    if null fs then putStrLn (iLine "[grpg] nothing to loot.")
    else do
      createDirectoryIfMissing True (userItemsDir you)
      mapM_ (\f -> do
               let src = ldir </> f
                   dst = userItemsDir you </> f
               _ <- try (renameFile src dst) :: IO (Either SomeException ())
               pure ()) fs
      putStrLn (iLine ("[grpg] looted " ++ show (length fs) ++ " item(s)."))
  renderMenu (sanitizeUser you) h p s

-- | Attack another present user. Winner collects loser’s items onto the floor.
doAttack :: String -> String -> String -> String -> String -> IO ()
doAttack you h p s targetRaw = do
  let target = sanitizeUser targetRaw
  pres <- listPresent h p s
  if target `notElem` pres
    then do putStrLn (iLine "[grpg] target not present."); renderMenu you h p s
    else do
      (as,ad,_) <- sumStats you
      (ds,dd,_) <- sumStats target
      let ahp0 = 5 + ad
          dhp0 = 5 + dd
          adm  = max 1 (as - dd)
          ddm  = max 1 (ds - ad)
          rk   = h ++ ":" ++ p ++ s
          leadAtt = (abs (hash32 ("lead:"++rk++":"++you++":"++target)) `mod` 2) == 0
          (aw, _dw) = fight leadAtt ahp0 dhp0 adm ddm
      if aw
        then do n <- dropAllToRoom target h p s
                appendLine (chatFile h p s) ("[combat] "++you++" defeated "++target
                  ++" ("++show as++"/"++show ad++" vs "++show ds++"/"++show dd++")"
                  ++ " dropped="++show n)
                putStrLn (iLine ("[grpg] you defeated "++target++"; they dropped "++show n++" item(s)."))
        else do n <- dropAllToRoom you h p s
                appendLine (chatFile h p s) ("[combat] "++target++" defeated "++you
                  ++" ("++show ds++"/"++show dd++" vs "++show as++"/"++show ad++")"
                  ++ " dropped="++show n)
                putStrLn (iLine ("[grpg] you were defeated by "++target++"; you dropped "++show n++" item(s)."))
      renderMenu you h p s
  where
    fight lead a d adm ddm
      | a <= 0 = (False, True)
      | d <= 0 = (True,  False)
      | lead   = fight False a (d - adm) adm ddm
      | otherwise = fight True (a - ddm) d adm ddm

-- | Move all items from a player → room loot. Returns count moved.
dropAllToRoom :: String -> String -> String -> String -> IO Int
dropAllToRoom who h p s = withRoomLock h p s $ do
  let idir = userItemsDir who
      ldir = roomLootDir h p s
  createDirectoryIfMissing True ldir
  ok <- doesDirectoryExist idir
  if not ok then pure 0 else do
    fs <- listDirectory idir
    moved <- mapM (\f -> do
                     let src = idir </> f
                         dst = ldir </> f
                     r <- try (renameFile src dst) :: IO (Either SomeException ())
                     pure (either (const False) (const True) r)
                  ) fs
    pure (length (filter id moved))

--------------------------------------------------------------------------------
-- Rendering (room header, presence, stats, loot summary, then proxied body)
--------------------------------------------------------------------------------

-- | Render the full proxied menu for a user in a room, with overlay info.
renderMenu :: String -> String -> String -> String -> IO ()
renderMenu you h p s = do
  spawnTreasureIfFirst h p s
  putStrLn $ iLine ("[grpg] " ++ you ++ " @ " ++ h ++ ":" ++ p ++ " " ++ s)
  ps <- listPresent h p s
  let others = filter (/= sanitizeUser you) ps
  putStrLn $ iLine ("[grpg] others here: " ++ if null others then "(no one)" else intercalate ", " others)
  (st,df,lk) <- sumStats you
  putStrLn $ iLine ("[grpg] you: STR="++show st++" DEF="++show df++" LUCK="++show lk)
  let cmdSel = cmdPrefix ++ sanitizeUser you ++ ":" ++ h ++ ":" ++ p ++ s
  putStrLn $ searchLine "[grpg] command: \"verb ...\" (say/look/loot/bag/attack <user>/pher host[:port]/sel)" cmdSel proxyHost
  lex <- doesDirectoryExist (roomLootDir h p s)
  n <- if lex then length <$> listDirectory (roomLootDir h p s) else pure 0
  if n > 0
    then do
      putStrLn $ iLine ("[grpg] loot on the floor: " ++ show n ++ " item(s) — use: loot")
      names <- roomLootNames h p s
      mapM_ (putStrLn . iLine . ("  - "++)) names
    else
      pure ()
  showChatTail h p s
  fetched <- fetchGopher h p s
  case fetched of
    Left _    -> putStrLn $ iLine "(fetch failed)"
    Right bod -> do
      let ls = lines bod
      if seemsMenu ls then mapM_ (putStrLn . rewriteItem you) ls
                      else mapM_ (putStrLn . iLine) ls
  putStrLn "."

-- | Like 'renderMenu', but anonymous (no stats/bag) and no presence.
renderMenuBare :: String -> String -> String -> IO ()
renderMenuBare h p s = do
  spawnTreasureIfFirst h p s
  putStrLn $ iLine ("[grpg] @ " ++ h ++ ":" ++ p ++ " " ++ s)
  let cmdSel = cmdPrefix ++ "anon:" ++ h ++ ":" ++ p ++ s
  putStrLn $ searchLine "[grpg] command (index search): \"<you>:verb ...\"" cmdSel proxyHost
  fetched <- fetchGopher h p s
  case fetched of
    Left _    -> putStrLn $ iLine "(fetch failed)"
    Right bod -> do
      let ls = lines bod
      if seemsMenu ls then mapM_ (putStrLn . rewriteItem "anon") ls
                      else mapM_ (putStrLn . iLine) ls
  putStrLn "."

--------------------------------------------------------------------------------
-- Commands (parsing + dispatch)
--------------------------------------------------------------------------------

data Cmd
  = Pher String String String String   -- ^ user, host, port, selector
  | Say  String String                 -- ^ user, message
  | Look String                        -- ^ user
  | Bare String String String          -- ^ host, port, selector
  | CmdExec String String String String String -- ^ (unused here)

-- | Parse a single-argument command (legacy convenience forms).
parseOneArg :: String -> Either String Cmd
parseOneArg raw0 =
  let s0       = percentDecode (trim raw0)
      hadProxy = "/proxy/" `isInfixOf` s0
      s1       = if hadProxy then stripToAfterProxy s0 else s0
      s2       = if hadProxy then dropUserPherPrefix s1 else s1
  in
  case break (==':') s2 of
    (uPart, rest0) | not (null rest0) ->
      let user = sanitizeUser uPart
          rest = trim (drop 1 rest0)
      in if null user then Left "invalid user"
         else case words rest of
           ("pher":uriWords) ->
             case parseHostPortSel (unwords uriWords) of
               Right (h,p,s) -> Right (Pher user h p s)
               Left e        -> Left e
           ("say":msgWords) ->
             if null msgWords then Left "usage: \"<you>:say <message>\""
             else Right (Say user (unwords msgWords))
           ("look":_)      -> Right (Look user)
           _               -> Left "unknown command; use pher/say/look"
    _ ->
      case parseLocBlob s2 of
        Just (h,p,s) -> Right (Bare h p s)
        Nothing ->
          case parseHostPortSel s2 of
            Right (h,p,s) -> Right (Bare h p s)
            Left e        -> Left e

--------------------------------------------------------------------------------
-- No-room fallback (when user has no last room recorded)
--------------------------------------------------------------------------------

-- | Inform the user they have no current room and offer a starting link.
renderNoRoom :: String -> IO ()
renderNoRoom you = do
  putStrLn $ iLine ("[grpg] " ++ you ++ " has no current room.")
  putStrLn $ iLine ("[grpg] choose a starting place:")
  let sel = proxyPrefix ++ sanitizeUser you ++ ":pher " ++ proxyHost ++ ":" ++ proxyPort ++ "/1/"
  putStrLn $ ('1' : "[enter gopherspace root]\t" ++ sel ++ "\t" ++ proxyHost ++ "\t" ++ proxyPort)
  putStrLn "."

--------------------------------------------------------------------------------
-- Verb runner (spawns loot on first visit; touches presence as needed)
--------------------------------------------------------------------------------

-- | Execute a verb in the given room context, then render.
runVerbInRoom :: String -> String -> String -> String -> String -> IO ()
runVerbInRoom you h p s verbStr =
  case words verbStr of
    []           -> do spawnTreasureIfFirst h p s
                       touchPresence you h p s
                       renderMenu you h p s
    ("say":rest) -> doSay you h p s (unwords rest)
    ("look":_)   -> do spawnTreasureIfFirst h p s
                       touchPresence you h p s
                       renderMenu you h p s
    ("bag":_)    -> do spawnTreasureIfFirst h p s
                       touchPresence you h p s
                       listTreasure you
                       renderMenu you h p s
    ("loot":_)   -> do spawnTreasureIfFirst h p s
                       touchPresence you h p s
                       doLoot you h p s
    ("attack":t:_) -> do spawnTreasureIfFirst h p s
                         touchPresence you h p s
                         doAttack you h p s t
    ("pher":uriWords) ->
      case parseHostPortSel (unwords uriWords) of
        Right (h2,p2,s2) -> do
          ensureChar you
          setLastRoom you h2 p2 s2
          spawnTreasureIfFirst h2 p2 s2
          touchPresence you h2 p2 s2
          renderMenu you h2 p2 s2
        Left _ -> do touchPresence you h p s
                     putStrLn $ iLine "[grpg] bad pher target"
                     renderMenu you h p s
    _ -> do touchPresence you h p s
            putStrLn $ iLine "[grpg] unknown verb; try: say <msg> | look | loot | attack <user> | pher host[:port]/sel"
            renderMenu you h p s

--------------------------------------------------------------------------------
-- Main entrypoint
--------------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  case args of
    -- Proxy a remote menu/page anonymously (no presence/bag).
    ("proxy":uriWords) | not (null uriWords) -> do
      case parseHostPortSel (unwords uriWords) of
        Right (h,p,s) -> renderMenuBare h p s
        Left e        -> die e

    -- Execute a verb for <user> in <room>.
    ("cmd":room:user:cmdWords) -> do
      case parseHostPortSel room of
        Right (h,p,s) -> do
          let you = sanitizeUser user
          ensureChar you
          setLastRoom you h p s
          runVerbInRoom you h p s (unwords cmdWords)
        Left e -> die e

    -- Venusia “cmd/*” form with explicit wildcard + search.
    [wildcard, search] -> do
      let blob = if "/gateway/games/grpg/cmd/" `isPrefixOf` wildcard
                   then drop (length "/gateway/games/grpg/cmd/") wildcard
                   else wildcard
      case parseCmdWildcard blob of
        Right (you,h,p,s) -> do
          ensureChar you
          setLastRoom you h p s
          runVerbInRoom (sanitizeUser you) h p s search
        Left e -> die e

    -- Legacy single-arg cmd form embedded in the path.
    [one] | "/gateway/games/grpg/cmd/" `isInfixOf` one -> do
      let s0  = percentDecode (trim one)
          s1  = stripToAfter "/cmd/" s0
      case splitLegacyCmdArg s1 of
        Left e -> die e
        Right (wl, search) ->
          case parseCmdWildcard wl of
            Right (you,h,p,s) -> do
              ensureChar you
              setLastRoom you h p s
              runVerbInRoom (sanitizeUser you) h p s search
            Left e2 -> die e2

    -- Single-arg “bare” or legacy verb forms.
    [one] -> do
      case parseOneArg one of
        Right (Bare h p s) -> renderMenuBare h p s
        Right (Pher you h p s) -> do
          ensureChar you
          setLastRoom you h p s
          spawnTreasureIfFirst h p s
          touchPresence you h p s
          renderMenu (sanitizeUser you) h p s
        Right (Say you msg) -> do
          ensureChar you
          m <- getLastRoom you
          case m of
            Nothing         -> renderNoRoom (sanitizeUser you)
            Just (hh,pp,ss) -> doSay (sanitizeUser you) hh pp ss msg
        Right (Look you) -> do
          ensureChar you
          m <- getLastRoom you
          case m of
            Nothing         -> renderNoRoom (sanitizeUser you)
            Just (hh,pp,ss) -> do
              spawnTreasureIfFirst hh pp ss
              touchPresence (sanitizeUser you) hh pp ss
              renderMenu (sanitizeUser you) hh pp ss
        Left e -> die e

    -- Usage help.
    _ -> die $ unlines
      [ "usage:"
      , "  grpg proxy <host[:port]><selector>"
      , "  grpg cmd   <host[:port]><selector> <user> <command...>"
      , ""
      , "  or single-arg legacy forms:"
      , "    \"<you>:pher host[:port]/selector\""
      , "    \"<you>:say <message>\""
      , "    \"<you>:look\""
      , ""
      , "verbs inside a room: say <msg> | look | loot | bag | attack <user> | pher host[:port]/sel"
      ]
