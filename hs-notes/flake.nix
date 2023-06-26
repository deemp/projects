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
      inherit (inputs.my-codium.lib.${system}) writeSettingsJSON mkCodium;
      inherit (inputs.drv-tools.lib.${system}) mkBin withAttrs withMan withDescription mkShellApps;
      inherit (inputs.my-codium.lib.${system}) extensions extensionsCommon settingsNix settingsCommonNix;
      inherit (inputs.devshell.lib.${system}) mkCommands mkRunCommands mkShell;
      inherit (inputs.haskell-tools.lib.${system}) toolsGHC;

      ghcVersion = "928";

      # and the name of the package
      developers-roadmap = "developers-roadmap";
      optics-by-example = "optics-by-example";
      sockets-and-pipes-notes = "sockets-and-pipes-notes";
      thinking-with-types = "thinking-with-types";
      make-docs = "make-docs";

      inherit (pkgs.haskell.lib)
        # dontCheck - skip tests
        dontCheck
        unmarkBroken
        # override deps of a package
        # see what can be overriden - https://github.com/NixOS/nixpkgs/blob/0ba44a03f620806a2558a699dba143e6cf9858db/pkgs/development/haskell-modules/generic-builder.nix#L13
        overrideCabal
        ;

      override = {
        overrides = self: super: {
          fused-effects-exceptions = dontCheck (self.callCabal2nix "fused-effects-exceptions" inputs.fused-effects-exceptions-src { });
          sockets-and-pipes = unmarkBroken super.sockets-and-pipes;
          aeson = super.aeson_2_1_2_1;
          ascii = unmarkBroken super.ascii_1_7_0_0;
          ascii-char = super.ascii-char_1_0_1_0;
          ascii-numbers = super.ascii-numbers_1_2_0_0;
          ascii-superset = super.ascii-superset_1_3_0_0;
          ascii-th = super.ascii-th_1_2_0_0;
          ascii-caseless = unmarkBroken super.ascii-caseless;
          ${developers-roadmap} = super.callCabal2nix developers-roadmap ./${developers-roadmap} { };
          ${optics-by-example} = super.callCabal2nix optics-by-example ./${optics-by-example} { };
          ${sockets-and-pipes-notes} = super.callCabal2nix sockets-and-pipes-notes ./${sockets-and-pipes-notes} { };
          ${thinking-with-types} = super.callCabal2nix thinking-with-types ./${thinking-with-types} { };
        };
      };

      inherit (toolsGHC {
        version = ghcVersion;
        inherit override;
        runtimeDependencies = [ ];
        # If we work on multiple packages, we need to supply all of them.
        # Suppose we develop packages A and B, where B is in deps of A.
        # GHC will be given dependencies of both A and B.
        # However, we don't want B to be in the list of deps of GHC
        # because build of GHC may fail due to errors in B.
        packages = ps: [
          ps.${developers-roadmap}
          ps.${optics-by-example}
          ps.${sockets-and-pipes-notes}
          ps.${thinking-with-types}
        ];
      })
        hls cabal implicit-hie fourmolu
        ghcid hpack ghc;


      packages = {
        codium = mkCodium { extensions = extensionsCommon // { inherit (extensions) haskell; }; };
        writeSettings = writeSettingsJSON (settingsCommonNix // {
          inherit (settingsNix) haskell;
          extra = {
            "haskell.plugin.fourmolu.config.external" = true;
          };
        });
      } // (mkShellApps {
        genDocs = {
          text = "${mkBin cabal} run make-docs";
          description = "Convert .hs to .md for mdbook";
        };
      });

      tools = [
        ghcid
        hpack
        cabal
        fourmolu
        hls
      ];

      devShells.default = mkShell {
        packages = tools;
        bash.extra = "export LANG=C.utf8";
        commands =
          (mkCommands "tools" tools) ++
          (mkRunCommands "ide" {
            "codium ." = packages.codium;
            inherit (packages) writeSettings;
          }) ++
          mkRunCommands "docs" {
            inherit (packages) genDocs;
          }
        ;
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
