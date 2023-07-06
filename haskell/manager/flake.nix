{
  inputs = { };

  outputs = inputs:
    let
      inputs_ =
        let haskell = (import ../.); in
        {
          inherit (haskell.outputs.inputs) devshell nixpkgs drv-tools flake-utils codium;
          inherit haskell;
        };

      outputs = flake { } // { inherit flake; inputs = inputs_; };

      flake =
        inputs__:
        let inputs = inputs_ // inputs__; in
        inputs.flake-utils.lib.eachDefaultSystem
          (system:
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
            })
              hls ghcid cabal fourmolu hpack
              justStaticExecutable callCabal2nix haskellPackages
              ;
            haskell-tools = [ hls ghcid cabal fourmolu hpack ];

            inherit (inputs.devshell.lib.${system}) mkCommands mkRunCommands mkShell;

            packageExecutableName = "manager";
            packageExe =
              let
                packageWithCompletion = haskellPackages.generateOptparseApplicativeCompletions [ packageExecutableName ] haskellPackages.${packageName};
                staticExecutable = justStaticExecutable { package = packageWithCompletion; };
              in
              withMan
                (withDescription staticExecutable (_: "Manage Haskell modules in a stack project (Alpha)"))
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
                mkCommands "tools" haskell-tools
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
              bash.extra = ''source ${packageExe}/share/bash-completion/completions/${packageExecutableName}'';
              commands =
                mkCommands "manager" [ packageExe ]
                ++ mkRunCommands "manager" { inherit (packages) default; };
            };
          in
          {
            inherit packages devShells;
          }
          ) // {
          templates = {
            init = {
              path = ./template;
              description = ''Used by `manager` to initialize a project'';
            };
          };
        };
    in
    outputs;

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
