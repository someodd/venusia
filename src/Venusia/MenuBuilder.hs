{- | Tools for building Gopher menus.

Simple, intuitive functions for creating Gopher menus

Intentionally not using rigid definition of types for the menu,
I have just found that annoying in the past.

-}
module Venusia.MenuBuilder
  ( -- * Menu creation functions
    menu
  , text
  , directory
  , search
  , binary
  , image
  , gif
  , html
  , info
  , error'
  -- * Customization
  , item
  -- * Rendering
  , render
  , gophermapRender
  ) where

import Venusia.Types
import Prelude hiding (error)
import qualified Data.Text as T
import Text.Read (readMaybe)

type ItemType = Char

type ItemHelper = T.Text -> Selector -> T.Text -> Int -> T.Text

crlf :: T.Text
crlf = "\r\n"

terminator :: T.Text
terminator = ".\r\n"

-- | Create a complete Gopher menu from menu items and render it
menu :: [T.Text] -> T.Text
menu items = T.concat items `T.append` terminator

-- | Create a standard text file item
text :: ItemHelper
text = item '0'

-- | Create a directory menu item
directory :: ItemHelper
directory = item '1'

-- | Create a search item
search :: ItemHelper
search = item '7'

-- | Create a binary file item
binary :: ItemHelper
binary = item '9'

-- | Create an image file item
image :: ItemHelper
image = item 'I'

-- | Create a GIF image item
gif :: ItemHelper
gif = item 'g'

-- | Create an HTML document item
html :: ItemHelper
html display url = item 'h' display ("URL:" <> url)

-- | Create an error item. It's named this way to avoid conflict with Prelude's error
-- function.
error' :: T.Text -> T.Text
error' msg = item '3' msg T.empty T.empty 0

-- | Create an information line (no selector)
info :: T.Text -> T.Text
info msg = item 'i' msg T.empty T.empty 0

-- | Create a custom item with any type character
item :: ItemType -> T.Text -> Selector -> T.Text -> Int -> T.Text
item typeChar display selector host port =
  T.singleton typeChar `T.append`
  display `T.append`
  T.singleton '\t' `T.append`
  selector `T.append`
  T.singleton '\t' `T.append`
  host `T.append`
  T.singleton '\t' `T.append`
  T.pack (show port) `T.append`
  crlf

-- | Alias for menu function
render :: [T.Text] -> T.Text
render = menu

{- | Render a .gophermap file.

Note that a .gophermap file can look like this:
  0Example text file	/fake
  1Example directory	/fake	localhost	7070
  hExample HTML	URL:http://example.com/Example.html
  Info line example

The output will look like this:
  0Example text file	/fake	localhost	7070
  1Example directory	/fake	localhost	7070
  hExample HTML	URL:http://example.com/Example.html
  iInfo line example	NULL	NULL	NULL
-}
gophermapRender :: T.Text -> Int -> T.Text -> T.Text
gophermapRender host port content =
  let items = map (parseLine . T.split (=='\t')) $ T.lines content
  in menu items
 where
  parseLine all@(itemTypeAndDisplay:selector:host:portText:_) =
    case T.uncons itemTypeAndDisplay of
      Just (itemType, display) ->
        case (readMaybe (T.unpack portText) :: Maybe Int) of
          Just p ->
            item itemType display selector host p
          Nothing ->
            info $ T.intercalate " " all
      Nothing -> info $ T.intercalate " " all
  parseLine all@[itemTypeAndDisplay, selector] =
    case T.uncons itemTypeAndDisplay of
      Just (itemType, display) ->
        item itemType display selector host port
      Nothing -> info $ T.intercalate " " all
  parseLine anythingElse =
    info $ T.intercalate " " anythingElse