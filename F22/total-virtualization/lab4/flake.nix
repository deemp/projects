{
  inputs = { };

  outputs = inputs:
    let
      inputs_ =
        let projects = import ../../..; in
        {
          inherit (projects.outputs.inputs) devshell nixpkgs flake-utils drv-tools;
        };

      outputs = outputs_ { } // { inputs = inputs_; outputs = outputs_; };

      outputs_ =
        inputs__:
        let inputs = inputs_ // inputs__; in
        inputs.flake-utils.lib.eachDefaultSystem
          (system:
          let
            pkgs = inputs.nixpkgs.legacyPackages.${system};
            inherit (inputs.devshell.lib.${system}) mkShell mkCommands mkRunCommands;
            inherit (inputs.drv-tools.lib.${system}) mkShellApps man getExe;
            packages = mkShellApps {
              bench = rec {
                # TODO fix "Reading the script from the standard input"
                text = ''${getExe pkgs.sysbench} --threads=2 --time=60 --cpu-max-prime=64000'';
                description = "task 1";
                longDescription = ''
                  ${man.DESCRIPTION}
                  ${description}
                '';
              };
            };

            tools = [ pkgs.sysbench ];

            devShells.default = mkShell {
              packages = tools;
              bash.extra = '''';
              commands =
                mkCommands "tools" tools
                ++ mkRunCommands "scripts" packages;
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
