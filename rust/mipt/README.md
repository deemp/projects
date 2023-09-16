# VSCodium generic

This flake provides a devshell with `VSCodium` a `hello` executable on `PATH` and with a couple of extensions.

## Prerequisites

<details>

  <summary>Spoiler</summary>

See these for additional info:

- [codium-generic](https://github.com/deemp/flakes/tree/main/templates/codium/generic#readme) - info just about `VSCodium` and extensions.
- [nix-vscode-extensions](https://github.com/nix-community/nix-vscode-extensions) (pinned [here](https://github.com/deemp/flakes/blob/main/source-flake/vscode-extensions/flake.nix)).
- [Prerequisites](https://github.com/deemp/flakes#prerequisites).
- [Troubleshooting](https://github.com/deemp/flakes/blob/main/README/Troubleshooting.md)

</details>

## Quick start

1. Install Nix - see [how](https://github.com/deemp/flakes/blob/main/README/InstallNix.md).

1. In a new terminal, start a devshell:

    ```console
    nix develop
    rustc
    ```

1. (Optionally) Write `settings.json` and start `VSCodium`:

    ```console
    nix run .#writeSettings
    nix run .#codium .
    ```

## Configs

- [.markdownlint.jsonc](./.markdownlint.jsonc) - for `markdownlint` from the extension `davidanson.vscode-markdownlint`
- [.envrc](./.envrc) - for [direnv](https://github.com/direnv/direnv)
