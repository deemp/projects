{
  inputs = { };
  outputs = inputs:
    let flakes = (import ../../.).outputs.inputs.flakes; in
    flakes.makeFlake {
      inputs = {
        inherit (flakes.all) nixpkgs drv-tools devshell haskell-tools;
        haskell = (import ../.);
      };
      perSystem = { inputs, system }:
        let
          pkgs = inputs.nixpkgs.legacyPackages.${system};
          inherit (inputs.haskell-tools.lib.${system}) toolsGHC;
          inherit (inputs.drv-tools.lib.${system}) withDescription withMan man;
          packageName = "manager";
          runtimeDependencies = [
            pkgs.coreutils
            pkgs.nix
            pkgs.git
            hpack
          ];

          inherit (inputs.haskell.toolsGHCPackage.${system} "manager" ./. {
            inherit runtimeDependencies;
          }) hls ghcid cabal fourmolu hpack haskellPackages;
          
          tools = [ hls ghcid cabal fourmolu hpack ];

          inherit (inputs.devshell.lib.${system}) mkCommands mkRunCommands mkShell;

          packageExecutableName = "manager";
          packageExe =
            let
              packageWithCompletion = haskellPackages.generateOptparseApplicativeCompletions [ packageExecutableName ] haskellPackages.${packageName};
            in
            withMan
              (withDescription packageWithCompletion (_: "Manage Haskell modules in a stack project (Alpha)"))
              (x: ''
                ${man.DESCRIPTION}
                ${x.meta.description}
              '')
          ;
          packages = {
            default = packageExe;
            package = haskellPackages.${packageName};
          };
          devShells.default = mkShell {
            commands =
              mkCommands "tools" tools
              ++ [
                {
                  name = "nix develop .#manager";
                  help = "Start a devshell with `manager`. Run `manager -h` there.";
                  category = "manager";
                }
              ];
          };
          devShells.manager = mkShell {
            packages = [ packageExe ];
            commands =
              mkCommands "manager" [ packageExe ]
              ++ mkRunCommands "manager" { inherit (packages) default; };
          };
        in
        {
          inherit packages devShells;
        };
      raw = _: {
        templates = {
          init = {
            path = ./template;
            description = ''Used by `manager` to initialize a project'';
          };
        };
      };
    };
  nixConfig = {
    extra-substituters = [
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
