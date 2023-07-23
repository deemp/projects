{
  inputs = { };
  outputs = inputs:
    let flakes = (import ../../.).outputs.inputs.flakes; in
    flakes.makeFlake {
      inputs = { inherit (flakes.all) nixpkgs drv-tools devshell codium python-tools; };
      perSystem = { inputs, system }:
        let
          pkgs = inputs.nixpkgs.legacyPackages.${system};
          inherit (inputs.codium.lib.${system}) mkCodium writeSettingsJSON;
          inherit (inputs.codium.lib.${system}) extensions extensionsCommon settingsNix settingsCommonNix;
          inherit (inputs.devshell.lib.${system}) mkShell mkCommands mkRunCommands;

          packages = {
            writeSettings = writeSettingsJSON (settingsCommonNix // { inherit (settingsNix) python; });
            createVenvs = inputs.python-tools.lib.${system}.createVenvs [ "lab5" "lab6" ];
            codium = mkCodium { extensions = extensionsCommon // { inherit (extensions) docker python kubernetes; }; };
          };

          tools = [
            pkgs.rabbitmq-server
            pkgs.hadolint
            pkgs.poetry
          ];

          devShells.default = mkShell {
            packages = tools;
            bash.extra = '''';
            commands =
              mkCommands "tools" tools
              ++ mkRunCommands "ide" {
                "codium ." = packages.codium;
                inherit (packages) writeSettings createVenvs;
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
