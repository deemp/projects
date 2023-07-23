{
  inputs = { };
  outputs = inputs:
    let flakes = (import ../.).outputs.inputs.flakes; in
    flakes.makeFlake {
      inputs = { inherit (flakes.all) nixpkgs drv-tools flakes-tools devshell codium; };
      perSystem = { inputs, system }:
        let
          pkgs = inputs.nixpkgs.legacyPackages.${system};
          inherit (inputs.codium.lib.${system}) mkCodium writeSettingsJSON;
          inherit (inputs.codium.lib.${system}) extensions extensionsCommon settingsNix settingsCommonNix;
          inherit (inputs.devshell.lib.${system}) mkCommands mkRunCommands mkShell;
          inherit (pkgs.lib.attrsets) mapAttrsToList;

          tools = [
            pkgs.postgresql_15
            pkgs.nodejs_18
          ];

          packages = {
            # --- IDE ---
            codium = mkCodium { extensions = extensionsCommon // { inherit (extensions) postgresql; }; };

            # a script to write `.vscode/settings.json`
            writeSettings = writeSettingsJSON (settingsCommonNix // { inherit (settingsNix) prettier-sql-vscode; });
          };

          devShells.default = mkShell {
            packages = tools;
            commands =
              mkCommands "tools" tools
              ++ mkRunCommands "ide" { "codium ." = packages.codium; inherit (packages) writeSettings; }
              ++ [{ name = "source microk8s.sh"; category = "env"; help = "set up environment"; }];
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
