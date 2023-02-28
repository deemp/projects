{
  inputs = {
    nixpkgs.follows = "haskell-tools/nixpkgs";
    flake-utils.follows = "haskell-tools/flake-utils";
    haskell-tools.url = "github:deemp/flakes?dir=language-tools/haskell";
    drv-tools.url = "github:deemp/flakes?dir=drv-tools";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs =
    { self
    , flake-utils
    , nixpkgs
    , haskell-tools
    , drv-tools
    , ...
    }:
    flake-utils.lib.eachDefaultSystem
      (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        inherit (haskell-tools.functions.${system}) toolsGHC;
        inherit (toolsGHC {
          version = "925";
        }) justStaticExecutable callCabal2nix hpack haskellPackages;
        inherit (drv-tools.functions.${system}) withDescription withMan withAttrs;
        inherit (drv-tools.configs.${system}) man;

        packageName = "manager";

        package = callCabal2nix packageName ./. { };

        runtimeDependencies = [
          pkgs.coreutils
          pkgs.nix
          pkgs.git
          hpack
        ];

        packageExecutableName = "manager";
        packageExe =
          let
            packageWithCompletion = haskellPackages.generateOptparseApplicativeCompletions [ packageExecutableName ] package;
            staticExecutable = justStaticExecutable { package = packageWithCompletion; };
          in
          withMan
            (withDescription staticExecutable (_: "Manage Haskell modules in a stack project. Run `manager -h`"))
            (x: ''
              ${man.DESCRIPTION}
              ${x.meta.description}
            '')
        ;
      in
      {
        packages = {
          default = packageExe;
          inherit package;
        };
        devShells.default = pkgs.mkShell {
          buildInputs = [ packageExe ];
          shellHook = ''
            source ${packageExe}/share/bash-completion/completions/${packageExecutableName}
          '';
        };
      }
      ) // {
      templates = {
        init = {
          path = ./template;
          description = ''Used by `manager` to initialize a project'';
        };
      };
    };

  nixConfig = {
    extra-substituters = [
      "https://haskell-language-server.cachix.org"
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
      "https://deemp.cachix.org"
    ];
    extra-trusted-public-keys = [
      "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "deemp.cachix.org-1:9shDxyR2ANqEPQEEYDL/xIOnoPwxHot21L5fiZnFL18="
    ];
  };
}
