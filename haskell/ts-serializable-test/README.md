# `ts-serializable` test

This project provides a generator of test cases for [ts-serializable](https://github.com/LabEG/Serializable).

The generator produces `TypeScript` classes with decorators from `ts-serializable` and testing code.

- generated classes: [index.ts](./ts-serializable/index.ts).
- generated JSON: [result.json](./ts-serializable/result.json)

## Quick start

1. Install Nix - see [how](https://github.com/deemp/flakes/blob/main/README/InstallNix.md).

1. In a new terminal, start a devshell and run the `Haskell` app. This command will generate [index.ts](./ts-serializable/index.ts).

    ```console
    nix develop
    cabal run
    ```

1. Run that `TypeScript` file.

    ```console
    cd ts-serializable
    npm i
    npx tsx index.ts
    ```

1. Write `settings.json` and start `VSCodium`.

    ```console
    nix run .#writeSettings
    nix run .#codium .
    ```

1. Open a `Haskell` file `app/Main.hs` and hover over a function.

1. Wait until `Haskell Language Server` (`HLS`) starts giving you type info.

## Configs

- [package.yaml] - used by `stack` or `hpack` to generate a `.cabal`
- [.markdownlint.jsonc](./.markdownlint.jsonc) - for `markdownlint` from the extension `davidanson.vscode-markdownlint`
- [.ghcid](./.ghcid) - for [ghcid](https://github.com/ndmitchell/ghcid)
- [.envrc](./.envrc) - for [direnv](https://github.com/direnv/direnv)
- [fourmolu.yaml](./fourmolu.yaml) - for [fourmolu](https://github.com/fourmolu/fourmolu#configuration)
- `hie.yaml` - not present, but can be generated via [implicit-hie](https://github.com/Avi-D-coder/implicit-hie) to verify the `Haskell Language Server` setup.
