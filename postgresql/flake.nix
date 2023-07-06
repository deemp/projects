{
  inputs = { };
  outputs = inputs:
    let
      inputs_ =
        let flakes = (import ../.).outputs.inputs.flakes; in
        {
          inherit (flakes.source-flake) nixpkgs flake-utils;
          inherit (flakes) drv-tools workflows flakes-tools devshell codium;
        };

      outputs = outputs_ { } // { inputs = inputs_; outputs = outputs_; };

      outputs_ =
        inputs__:
        let inputs = inputs_ // inputs__; in
        inputs.flake-utils.lib.eachDefaultSystem
          (system:
          let
            pkgs = inputs.nixpkgs.legacyPackages.${system};
            inherit (inputs.codium.lib.${system}) mkCodium writeSettingsJSON;
            inherit (inputs.codium.lib.${system}) extensions extensionsCommon settingsNix settingsCommonNix;
            inherit (inputs.devshell.lib.${system}) mkCommands mkRunCommands mkShell;

            tools = [
              pkgs.postgresql_15
              pkgs.nodejs_18
            ];

            packages = {
              # --- IDE ---
              codium = mkCodium { extensions = extensionsCommon // { inherit (extensions) postgresql; }; };

              # a script to write `.vscode/settings.json`
              writeSettings = writeSettingsJSON (settingsCommonNix // { inherit (extensions) prettier-sql-vscode; });
            };

            devShells.default = mkShell {
              packages = tools;
              bash.extra = ''source ${./microk8s.sh}'';
              commands =
                mkCommands "tools" tools
                ++ mkRunCommands "ide" { "codium ." = packages.codium; inherit (packages) writeSettings; };
            };
          in
          {
            inherit packages devShells;
          });
    in
    outputs;

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
