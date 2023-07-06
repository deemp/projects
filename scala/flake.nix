{
  inputs = { };

  outputs = inputs:
    let
      inputs_ =
        let flakes = (import ../.).outputs.inputs.flakes; in
        {
          inherit (flakes.source-flake) nixpkgs flake-utils;
          inherit (flakes) devshell codium;
          python-tools = flakes.language-tools.python;
        };

      outputs = outputs_ { } // { inputs = inputs_; outputs = outputs_; };

      outputs_ =
        inputs__:
        let inputs = inputs_ // inputs__; in
        inputs.flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = inputs.nixpkgs.legacyPackages.${system};
          inherit (inputs.codium.lib.${system}) writeSettingsJSON mkCodium extensions extensionsCommon settingsCommonNix;
          inherit (inputs.devshell.lib.${system}) mkShell mkCommands mkRunCommands;

          packages = {
            codium = mkCodium {
              extensions = extensionsCommon // { inherit (extensions) scala; };
            };

            writeSettings = writeSettingsJSON settingsCommonNix;
          };

          tools = [
            pkgs.sbt
          ];

          devShells.default = mkShell {
            packages = tools;
            bash.extra = '''';
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
    extra-substituters = [
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
