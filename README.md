# O Venezia, Venaga, Venusia

**Venusia** is a fast and flexible Haskell library and daemon for building servers for the [Internet Gopher Protocol](https://en.wikipedia.org/wiki/Gopher_\(protocol\)). It's designed for ease of use, powerful configuration, and extensibility.

It works well with other tools in its ecosystem:

  * **Build:** Use [Bore](https://github.com/someodd/bore) for static site generation (phlogs, templates) for your gopherspace.
  * **Rank:** Use [RYVM](https://github.com/someodd/ryvm) to rank files for search results.

## Features

Venusia is a complete framework for creating modern Gopherholes.

#### As a Library

  * **Declarative Routing:** Easily define routes with support for wildcards.
  * **Built-in Handlers:** Ready-to-use handlers for directories, files, and search.
  * **`.gophermap` Support:** Automatically serves `.gophermap` files for custom menus.
  * **Menu Builder:** An intuitive DSL for creating Gopher menus.

#### As a Daemon (`venusia-exe`)

  * **TOML Configuration:** Use a simple `routes.toml` to configure file servers, search, and gateways.
  * **Hot Reloading:** Watches your directory and `routes.toml`, reloading automatically on changes with no downtime.
  * **Process Gateways:** Execute external commands (e.g., `cowsay`, `figlet`, `curl`) and serve their output.

## Getting Started

### Running the Daemon

The quickest way to start is with the `venusia-exe` daemon.

1.  **Create a `routes.toml` file:**

    This file configures your Gopher server's behavior.

    ```toml
    # routes.toml

    # Serves files from the current directory at the /files/ selector.
    # NOTE: Do not use a wildcard (*) in the files selector.
    [[files]]
    selector = "/files/"
    path = "./"

    # Creates a search endpoint for the current directory.
    [[search]]
    selector = "/search"
    path = "./"

    # Gateway to the 'cowsay' command.
    [[gateway]]
    selector = "/gateway/cowsay"
    command = "cowsay"
    arguments = ["$search"] # Takes a search query
    menu = true

    # Gateway to the 'curl' command for weather.
    [[gateway]]
    selector = "/gateway/weather"
    command = "curl"
    arguments = ["https://wttr.in/$search?format=v2"]
    menu = true
    ```

2.  **Run the `watch` command:**

    Point the watcher to your directory, host, and port.

    ```bash
    venusia watch /path/to/your/gopherhole gopher.example.com 7070
    ```

    Your Gopher server is now live. Changes to your files or `routes.toml` will be reflected instantly.

### Using the Library

To build a custom server, use Venusia in your own Haskell project.

1.  **Add the dependency** in `package.yaml`:

    ```yaml
    dependencies:
    - Venusia
    ```

2.  **Create your server:**

    ```haskell
    -- app/Main.hs
    module Main (main) where

    import Venusia.Server
    import Venusia.MenuBuilder
    import Venusia.FileHandler
    import Venusia.SearchHandler
    import qualified Data.Text as T

    host :: T.Text
    host = "localhost"

    port :: Int
    port = 7070

    -- Define server routes
    routes :: [Route]
    routes =
      [ on "/hello" $ \_ ->
          return $ TextResponse "Hello, gopher!\r\n"
      , onWildcard "/echo/*" $ \req ->
          pure $ TextResponse $ req.reqWildcard & fromMaybe "Nothing."
      , on "/search" (handleSearch "./" host port)
      , onWildcard "/files/*" $ \req ->
          case req.reqWildcard of
            Just path -> serveDirectory host port "/var/gopher" "/files/" path Nothing
            Nothing   -> pure $ TextResponse "No path provided."
      ]

    -- Main entry point
    main :: IO ()
    main = serve (show port) noMatchHandler routes
    ```

## Debian Packages & `systemd`

For production, Venusia offers Debian packages with `systemd` support to run as a managed daemon.

After installing a package, you can edit the service configuration at `/lib/systemd/system/venusia.service`. For example, to integrate with `bore`, you could change the `ExecStart` line to:

```
ExecStart=/usr/bin/venusia watch /var/gopher/source gopher.someodd.zip 7071 "/usr/bin/bore build --source /var/gopher/source --output /var/gopher/output" 10000000
```

Then, reload the `systemd` daemon and restart the service:

```
sudo systemctl daemon-reload
sudo systemctl restart venusia.service
```

## Configuration (`routes.toml`)

Configure the daemon by defining routes in `routes.toml`.

  * **`[[files]]`**: Serves static files.
      * `selector`: Gopher path prefix (e.g., `/files/`).
      * `path`: Local directory to serve.
  * **`[[search]]`**: Provides a search endpoint.
      * `selector`: Gopher path for the query.
      * `path`: Local directory to index and search.
  * **`[[gateway]]`**: Executes a shell command.
      * `selector`: Gopher path, can include a wildcard (`*`).
      * `command`: The command to run.
      * `arguments`: Command arguments. `$search` and `$wildcard` are replaced with user input.
      * `wildcard`: Set to `true` if the selector uses a wildcard.
      * `menu`: Set to `true` to format the command's output as a Gopher menu.