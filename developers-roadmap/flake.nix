{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    my-codium.url = "github:deemp/flakes?dir=codium";
    drv-tools.url = "github:deemp/flakes?dir=drv-tools";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    haskell-tools.url = "github:deemp/flakes?dir=language-tools/haskell";
    devshell.url = "github:deemp/flakes?dir=devshell";
    flakes-tools.url = "github:deemp/flakes?dir=flakes-tools";
    fused-effects-exceptions-src = {
      url = "github:fused-effects/fused-effects-exceptions";
      flake = false;
    };
    lima.url = "github:deemp/flakes?dir=lima";
  };
  outputs =
    { self
    , flake-utils
    , flakes-tools
    , nixpkgs
    , my-codium
    , drv-tools
    , haskell-tools
    , devshell
    , fused-effects-exceptions-src
    , lima
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (my-codium.functions.${system}) writeSettingsJSON mkCodium;
      inherit (drv-tools.functions.${system}) mkBin withAttrs withMan withDescription mkShellApp;
      inherit (drv-tools.configs.${system}) man;
      inherit (my-codium.configs.${system}) extensions settingsNix;
      inherit (flakes-tools.functions.${system}) mkFlakesTools;
      inherit (devshell.functions.${system}) mkCommands mkShell;
      inherit (haskell-tools.functions.${system}) toolsGHC;

      ghcVersion_ = "925";

      # and the name of the package
      myPackageName = "nix-managed";

      # Then, we list separately the libraries that our package needs
      myPackageDepsLib = [ ];

      # And the binaries. 
      # In our case, the Haskell app will call the `hello` command
      myPackageDepsBin = [ ];

      inherit (pkgs.haskell.lib)
        # doJailbreak - remove package bounds from build-depends of a package
        doJailbreak
        # dontCheck - skip tests
        dontCheck
        # override deps of a package
        # see what can be overriden - https://github.com/NixOS/nixpkgs/blob/0ba44a03f620806a2558a699dba143e6cf9858db/pkgs/development/haskell-modules/generic-builder.nix#L13
        overrideCabal
        ;

      override = {
        overrides = self: super: {
          fused-effects-exceptions = dontCheck (self.callCabal2nix "fused-effects-exceptions" fused-effects-exceptions-src { });
          myPackage = overrideCabal
            (super.callCabal2nix myPackageName ./. { })
            (x: {
              # we can combine the existing deps and new deps
              # these deps will be in haskellPackages.myPackage.getCabalDeps.librarySystemDepends
              librarySystemDepends = myPackageDepsLib ++ (x.librarySystemDepends or [ ]);
              # if we want to override the existing deps, we just don't include them
              executableSystemDepends = myPackageDepsBin ++ (x.executableSystemDepends or [ ]);
              # here's how we can add a package built from sources
              # then, we may use this package in .cabal in a test-suite
              testHaskellDepends = [
                (super.callCabal2nix "lima" "${lima.outPath}/lima" { })
              ] ++ (x.testHaskellDepends or [ ]);
            });
        };
      };

      inherit (toolsGHC {
        version = ghcVersion_;
        inherit override;
        packages = (ps: [ ps.myPackage ]);
      })
        hls cabal implicit-hie justStaticExecutable
        ghcid callCabal2nix haskellPackages hpack;

      writeSettings = writeSettingsJSON {
        inherit (settingsNix) haskell todo-tree files editor gitlens
          git nix-ide workbench markdown-all-in-one markdown-language-features
          yaml;
      };

      codiumTools = [
        ghcid
        hpack
        implicit-hie
        cabal
        hls
      ];

      codium = mkCodium {
        extensions = { inherit (extensions) nix haskell misc github markdown; };
        runtimeDependencies = codiumTools;
      };

      tools = codiumTools;

      defaultShell = mkShell {
        packages = tools;
        bash.extra = "export LANG=C.utf8";
        commands = (mkCommands "tools" tools) ++ [
          {
            name = "mkdocs";
            category = "docs";
            help = "generate docs (`README.md`, etc.)";
            command = "cabal v1-test";
          }
          {
            name = "nix run .#codium .";
            category = "ide";
            help = "Run " + codium.meta.description + " in the current dir";
          }
          {
            name = "nix run .#writeSettings";
            category = "ide";
            help = writeSettings.meta.description;
          }
        ];
      };
    in
    {
      packages = {
        inherit writeSettings codium;
      };

      devShells = {
        default = defaultShell;
      };
    });

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
