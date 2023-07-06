{
  inputs = { };

  outputs = inputs:
    let
      inputs_ =
        let flakes = (import ../../..).outputs.inputs.flakes; in
        {
          inherit (flakes.source-flake) nixpkgs flake-utils;
          inherit (flakes) devshell drv-tools;
          python-tools = flakes.language-tools.python;
        };

      outputs = outputs_ { } // { inputs = inputs_; outputs = outputs_; };

      outputs_ =
        inputs__:
        let inputs = inputs_ // inputs__; in
        inputs.flake-utils.lib.eachDefaultSystem
          (system:
          let
            pkgs = inputs.nixpkgs.legacyPackages.${system};
            devshell = inputs.devshell.devshell.${system};
            inherit (inputs.devshell.lib.${system}) mkCommands;
            inherit (inputs.drv-tools.lib.${system}) mkShellApps withAttrs;
            inherit (inputs.python-tools.lib.${system}) activateVenv;
            tools = [
              pkgs.docker
              pkgs.poetry
            ];
          in
          {
            devShells.default = devshell.mkShell
              {
                packages = tools;
                bash.extra = activateVenv;
                commands = mkCommands "tools" tools;
              };
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
