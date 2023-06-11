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
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = inputs.nixpkgs.legacyPackages.${system};
      inherit (inputs.my-codium.functions.${system}) writeSettingsJSON mkCodium;
      inherit (inputs.drv-tools.functions.${system}) mkBin withAttrs withMan withDescription mkShellApp;
      inherit (inputs.drv-tools.configs.${system}) man;
      inherit (inputs.my-codium.configs.${system}) extensions extensionsCommon settingsNix settingsCommonNix;
      inherit (inputs.devshell.functions.${system}) mkCommands mkRunCommands mkShell;
      inherit (inputs.haskell-tools.functions.${system}) toolsGHC;

      ghcVersion = "928";

      # and the name of the package
      packageName = "nix-managed";

      inherit (pkgs.haskell.lib)
        # dontCheck - skip tests
        dontCheck
        # override deps of a package
        # see what can be overriden - https://github.com/NixOS/nixpkgs/blob/0ba44a03f620806a2558a699dba143e6cf9858db/pkgs/development/haskell-modules/generic-builder.nix#L13
        overrideCabal
        ;

      override = {
        overrides = self: super: {
          fused-effects-exceptions = dontCheck (self.callCabal2nix "fused-effects-exceptions" inputs.fused-effects-exceptions-src { });
          ${packageName} = super.callCabal2nix packageName ./. { };
        };
      };

      packages = {
        codium = mkCodium { extensions = extensionsCommon // { inherit (extensions) haskell; }; };
        writeSettings = writeSettingsJSON (settingsCommonNix // { inherit (settingsNix) haskell; });
      };

      hpkgs = pkgs.haskell.packages."ghc${ghcVersion}";

      devShells.shellFor = (hpkgs.override override).shellFor {
        packages = ps: [ ps.${packageName} ];
        buildInputs = [ pkgs.cabal-install ];
      };

      tools = [
        pkgs.ghcid
        pkgs.hpack
        pkgs.cabal-install
        pkgs.haskellPackages.fourmolu_0_12_0_0
        hpkgs.haskell-language-server
      ];

      devShells.default = mkShell {
        packages = tools;
        packagesFrom = [ devShells.shellFor ];
        bash.extra = "export LANG=C.utf8";
        commands = (mkCommands "tools" tools) ++ (mkRunCommands "ide" {
          "codium ." = packages.codium;
          inherit (packages) writeSettings;
        }) ++
        [
          {
            name = "mkdocs";
            category = "docs";
            help = "generate docs (`README.md`, etc.)";
            command = "cabal v1-test";
          }
        ];
      };
    in
    {
      inherit devShells packages;
    });

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
