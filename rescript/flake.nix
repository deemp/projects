{
  outputs = inputs:
    let flakes = (import ../.).outputs.inputs.flakes; in
    flakes.makeFlake {
      inputs = { inherit (flakes.all) nixpkgs devshell codium python-tools; };
      perSystem = { inputs, system }:
        let
          pkgs = inputs.nixpkgs.legacyPackages.${system};
          inherit (inputs.codium.lib.${system}) mkCodium writeSettingsJSON extensionsCommon extensions settingsNix settingsCommonNix;
          inherit (inputs.devshell.lib.${system}) mkCommands mkRunCommands mkShell;

          packages = {
            # --- IDE ---

            # We compose `VSCodium` with extensions
            codium = mkCodium {
              extensions = extensionsCommon // { inherit (extensions) rescript; };
            };

            # a script to write `.vscode/settings.json`
            writeSettings = writeSettingsJSON (settingsCommonNix // {
              inherit (settingsNix) rescript-vscode explorer;
              extra = {
                "python.defaultInterpreterPath" = "\${workspaceFolder}/.venv/bin/python";
              };
            });
          };

          tools = [ pkgs.nodejs_18 pkgs.poetry ];

          devShells.default = mkShell {
            packages = tools;
            bash.extra = "hello";
            commands =
              mkCommands "tools" tools
              ++ mkRunCommands "ide" {
                "codium ." = packages.codium;
                inherit (packages) writeSettings;
              };
          };
        in
        {
          inherit packages devShells;
        };
    };

  nixConfig = {
    extra-trusted-substituters = [
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
      "https://deemp.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "deemp.cachix.org-1:9shDxyR2ANqEPQEEYDL/xIOnoPwxHot21L5fiZnFL18="
    ];
  };
}
