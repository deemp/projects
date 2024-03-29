{
  outputs = inputs:
    let flakes = (import ../.).outputs.inputs.flakes; in
    flakes.makeFlake {
      inputs = {
        inherit (flakes.all)
          nixpkgs formatter nix-vscode-extensions drv-tools
          devshell codium python-tools;
      };
      perSystem = { inputs, system }:
        let
          pkgs = inputs.nixpkgs.legacyPackages.${system};
          inherit (inputs.codium.lib.${system}) settingsNix mkCodium writeSettingsJSON extensionsCommon extensions;
          inherit (inputs.drv-tools.lib.${system}) mkShellApp;
          inherit (inputs.python-tools.lib.${system}) activateVenv;
          inherit (inputs.devshell.lib.${system}) mkShell mkCommands mkRunCommands;
          inherit (inputs.nix-vscode-extensions.extensions.${system}) vscode-marketplace;

          packages = {
            codium = mkCodium {
              extensions = extensionsCommon // {
                inherit (extensions) python sql;
                other = {
                  inherit (vscode-marketplace.nomicfoundation) hardhat-solidity;
                };
              };
            };
            writeSettings = writeSettingsJSON settingsNix;
            createVenvs = inputs.python-tools.lib.${system}.createVenvs [ "." ];
          };

          tools = [
            pkgs.docker
            pkgs.poetry
            pkgs.rustup
            pkgs.nodePackages.near-cli
          ];
          devShells.default = mkShell {
            bash.extra = ''
              ${pkgs.lib.getExe packages.createVenvs}
              ${activateVenv}
            '';
            packages = tools;
            commands =
              mkCommands "tools" tools
              ++ mkRunCommands "ide" { "codium ." = packages.codium; inherit (packages) createVenvs; };
          };
        in
        {
          inherit packages devShells;
        };
    };

  nixConfig = {
    extra-trusted-substituters = [
      "https://nix-community.cachix.org"
      "https://hydra.iohk.io"
      "https://deemp.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "deemp.cachix.org-1:9shDxyR2ANqEPQEEYDL/xIOnoPwxHot21L5fiZnFL18="
    ];
  };
}
