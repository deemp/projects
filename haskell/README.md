# Developers roadmap

Source code for this `README.md` is in [README.hs](README.hs).

## Prerequisites

- [codium-generic](https://github.com/deemp/flakes/tree/main/templates/codium/generic#readme) - info just about `VSCodium` and extensions.
- [flake.nix](./flake.nix) - extensively commented code.
- [Haskell](https://github.com/deemp/flakes/blob/main/README/Haskell.md)
- [Troubleshooting](https://github.com/deemp/flakes/blob/main/README/Troubleshooting.md)
- [Prerequisites](https://github.com/deemp/flakes#prerequisites)

## Quick start

1. Start a devshell.

  ```console
  nix develop
  ```

1. (Optionally) Start `VSCodium`.

  ```console
  nix run .#writeSettings
  nix run .#codium .
  ```

1. Open a [README.hs](optics-by-example/README.hs) and hover over a function. `Haskell Language Server` should start giving you type information.

1. Generate docs for the top [mdbook](../docs).

  ```console
  nix run .#genDocs
  ```

## Projects

This flake provides tools for `developers-roadmap`,`optics-by-example`, `sockets-and-pipes-notes`,
`thinking-with-types`.

Other directories have their own flakes.
