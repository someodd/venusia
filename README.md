# O Venezia, Venaga, Venusia

Venusia is a Haskell library for buildling [Internet Gopher Protocol](https://en.wikipedia.org/wiki/Gopher_(protocol)) servers and handling gophermaps/menus.

## WARNING: in alpha

This is currently experimental and the API will be constantly twisted to suit my needs (as a dependency) until the first release.

## Example

Look at [Main.hs](app/Main.hs) for an example!

## Known issues

weird issue:

```
➜  gopherhole_bore curl "gopher://localhost:7070/1/files/../"     
3Not found: /			0
```

the `..` does this