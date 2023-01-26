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
      inherit (haskell-tools.functions.${system}) haskellTools;

      ghcVersion_ = "92";
      myPackageName = "nix-managed";
      myPackageDepsBin = [ ];
      override = {
        overrides = self: super: {
          myPackage = (super.callCabal2nix myPackageName ./. { });
        };
      };

      inherit (haskellTools ghcVersion_ override (ps: [ ps.myPackage ]) myPackageDepsBin)
        stack hls cabal implicit-hie justStaticExecutable
        ghcid callCabal2nix haskellPackages hpack;


      writeSettings = writeSettingsJSON {
        inherit (settingsNix) haskell todo-tree files editor gitlens
          git nix-ide workbench markdown-all-in-one markdown-language-features
          yaml;
        other = {
          "Prettier-SQL.SQLFlavourOverride" = "postgresql";
          "Prettier-SQL.indentStyle" = "tabularLeft";
          "Prettier-SQL.keywordCase" = "upper";
          "Prettier-SQL.expressionWidth" = 42;
          "[sql]" = {
            "editor.defaultFormatter" = "inferrinizzard.prettier-sql-vscode";
          };
        };
      };

      codiumTools = [
        ghcid
        hpack
        implicit-hie
        cabal
        pkgs.minikube
        pkgs.kubectl
        pkgs.postgresql_15
        pkgs.helm
      ];

      # And compose VSCodium with dev tools and HLS
      codium = mkCodium {
        extensions = { inherit (extensions) nix haskell misc github markdown kubernetes postgresql; };
        runtimeDependencies = codiumTools ++ [ hls ];
      };

      tools = codiumTools ++ [ codium ];

      defaultShell = mkShell {
        packages = tools;
        bash.extra = ''LANG=C.utf8'';
        commands = mkCommands "tools" tools;
      };

      # --- flakes tools ---
      # Also, we provide scripts that can be used in CI
      flakesTools = mkFlakesTools [ "." ];
    in
    {
      packages = {
        inherit (flakesTools) updateLocks pushToCachix;
        inherit writeSettings;
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
