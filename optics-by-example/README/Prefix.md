# Lens

Notes on optics implemented in `Haskell`.

## Dev tools

### Prerequisites

- [codium-generic](https://github.com/deemp/flakes/tree/main/templates/codium/generic#readme) - info just about `VSCodium` and extensions.
- [flake.nix](./flake.nix) - extensively commented code.
- [Haskell](https://github.com/deemp/flakes/blob/main/README/Haskell.md)
- [Troubleshooting](https://github.com/deemp/flakes/blob/main/README/Troubleshooting.md)
- [Prerequisites](https://github.com/deemp/flakes#prerequisites)

### Quick start

1. Install Nix - see [how](https://github.com/deemp/flakes/blob/main/README/InstallNix.md).

2. In a new terminal, start a devshell.

    ```console
    nix develop
    cabal run
    ```

3. (Optionally) Write `settings.json` and start `VSCodium`.

    ```console
    nix run .#writeSettings
    nix run .#codium .
    ```

4. Open a `Haskell` file and wait until `Haskell Language Server` starts giving type info.

### Configs

- [package.yaml](./package.yaml) - used by `hpack` to generate a `.cabal`.
- [.markdownlint.jsonc](./.markdownlint.jsonc) - for `markdownlint` from the extension `davidanson.vscode-markdownlint`.
- [.ghcid](./.ghcid) - for [ghcid](https://github.com/ndmitchell/ghcid).
- [.envrc](./.envrc) - for [direnv](https://github.com/direnv/direnv).
- [fourmolu.yaml](./fourmolu.yaml) - for [fourmolu](https://github.com/fourmolu/fourmolu#configuration).
- [.github/workflows/ci.yaml] - a generated `GitHub Actions` workflow. See [workflows](https://github.com/deemp/flakes/tree/main/workflows). Generate a workflow via `nix run .#writeWorkflows`.
- `hie.yaml` - not present, but can be generated via [implicit-hie](https://github.com/Avi-D-coder/implicit-hie) (available on devshell) to verify the `Haskell Language Server` setup.
