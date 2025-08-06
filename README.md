# O Venezia, Venaga, Venusia

Venusia is a Haskell library for buildling [Internet Gopher Protocol](https://en.wikipedia.org/wiki/Gopher_(protocol)) servers and handling gophermaps/menus.

## Library features

Create Gopher servers which:

  * Serve directories
  * Provide search interfaces
  * Interact with other programs

And more!

## Daemon

Venusia is also an Internet Gopher Protocol daemon, so you can use it like that too.

Features:

  * Gateway configuration: use a simple TOML file to configure "gateways"
  * Watch server: watch a directory and reload when it changes or when the gateways change
  * `systemd` integration

Once you install the Debian package just edit the `systemd` config and reload it.

## WARNING: in alpha

This is currently experimental and the API will be constantly twisted to suit my needs (as a dependency) until the first release.

## Example

Look at [./examples/](./examples/) to see how to use this library!

## Known issues

weird issue:

```
➜  gopherhole_bore curl "gopher://localhost:7070/1/files/../"     
3Not found: /			0
```

the `..` does this