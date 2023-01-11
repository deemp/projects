# Haskell

`VSCodium` with extensions and executables for `Haskell`

## Prerequisites

- [flake.nix](./flake.nix) - extensively commented code
- [Prerequisites](https://github.com/deemp/flakes#prerequisites)
- [Haskell](https://github.com/deemp/flakes/blob/main/README/Haskell.md)
- [Troubleshooting](https://github.com/deemp/flakes/blob/main/README/Troubleshooting.md) - see in case of problems with VSCodium, etc.

## Quick start

1. Install Nix - see [how](https://github.com/deemp/flakes/blob/main/README/InstallNix.md).

1. In a new terminal, run `VSCodium` from a devshell:

```console
nix develop
write-settings-json
codium .
```

1. Open a `Haskell` file and hover over a function.

1. Wait until `Haskell Language Server` (`HLS`) starts giving you type info.

## Cabal

```console
nix develop
cabal run
```

### GHC

This template uses `GHC 9.2`. You can switch to `GHC 9.0`:

- In `flake.nix`, change `"92"` to `"90"`

## References

- [Esqueleto and complex queries](https://mmhaskell.com/real-world/esqueleto)
  - Database - [src](https://github.com/MondayMorningHaskell/RealWorldHaskell/blob/master/src/Database.hs)

- [Database access](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/persistent-db)
- `Persistent` with `PostgreSQL` backend - [src](https://www.yesodweb.com/book-1.4/persistent#persistent_something_besides_sqlite)
  - [Code generation](https://www.yesodweb.com/book-1.4/persistent#persistent_code_generation) - associated data types showcase
- `book` table - from [here](https://stepik.org/lesson/308886/step/1?unit=291012)
