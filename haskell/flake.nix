{
  inputs = {
    fused-effects-exceptions = {
      url = "github:fused-effects/fused-effects-exceptions";
      flake = false;
    };
    large-records = {
      url = "github:well-typed/large-records";
      flake = false;
    };
  };

  outputs = inputs:
    let
      inputs_ =
        let flakes = (import ../.).outputs.inputs.flakes; in
        {
          inherit (flakes.source-flake) nixpkgs flake-utils;
          inherit (flakes) drv-tools devshell codium;
          haskell-tools = flakes.language-tools.haskell;
          inherit (inputs) fused-effects-exceptions large-records;
        };

      outputs = outputs_ { } // { inputs = inputs_; outputs = outputs_; };

      outputs_ =
        inputs__:
        let inputs = inputs_ // inputs__; in
        inputs.flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = inputs.nixpkgs.legacyPackages.${system};
          inherit (inputs.codium.lib.${system}) writeSettingsJSON mkCodium;
          inherit (inputs.drv-tools.lib.${system}) getExe withAttrs withMan withDescription mkShellApps mapGenAttrs;
          inherit (inputs.codium.lib.${system}) extensions extensionsCommon settingsNix settingsCommonNix;
          inherit (inputs.devshell.lib.${system}) mkCommands mkRunCommands mkShell;
          inherit (inputs.haskell-tools.lib.${system}) toolsGHC;

          ghcVersion = "928";

          packageNames = [
            "developers-roadmap"
            "optics-by-example"
            "sockets-and-pipes-notes"
            "thinking-with-types"
          ];

          make-docs = "make-docs";

          inherit (pkgs.haskell.lib)
            # skip tests
            dontCheck
            # remove `broken` flag
            unmarkBroken
            overrideCabal
            ;

          override = {
            overrides = self: super: {
              fused-effects-exceptions = dontCheck (super.callCabal2nix "fused-effects-exceptions" inputs.fused-effects-exceptions { });
              sockets-and-pipes = unmarkBroken super.sockets-and-pipes;
              aeson = super.aeson_2_1_2_1;
              ascii = unmarkBroken super.ascii_1_7_0_0;
              ascii-char = super.ascii-char_1_0_1_0;
              ascii-numbers = super.ascii-numbers_1_2_0_0;
              ascii-superset = super.ascii-superset_1_3_0_0;
              ascii-th = super.ascii-th_1_2_0_0;
              ascii-caseless = unmarkBroken super.ascii-caseless;
              large-anon = super.callCabal2nixWithOptions "large-anon" "${inputs.large-records.outPath}/large-anon" "-f disablefourmoluexec" { };
              typelet = unmarkBroken super.typelet;
              large-generics = super.callCabal2nix "large-generics" "${inputs.large-records.outPath}/large-generics" { };
            } // (mapGenAttrs (name: { "${name}" = super.callCabal2nix name ./${name} { }; }) packageNames);
          };

          inherit
            (toolsGHC {
              version = ghcVersion;
              inherit override;
              packages = ps: builtins.map (name: ps.${name}) packageNames;
            }) ghc hls ghcid cabal fourmolu hpack;

          toolsGHCPackage = name: path: args@{ ... }:
            toolsGHC ({
              version = ghcVersion;
              override = { overrides = self: super: { ${name} = super.callCabal2nix name path { }; }; };
              packages = ps: [ ps.${name} ];
            } // args);

          packages = {
            codium = mkCodium { extensions = extensionsCommon // { inherit (extensions) haskell; }; };
            writeSettings = writeSettingsJSON (settingsCommonNix // { inherit (settingsNix) haskell; });
          } // (mkShellApps {
            genDocs = {
              text = "LANG=C.utf8 ${getExe (toolsGHCPackage "make-docs" ./make-docs { }).haskellPackages.make-docs}";
              description = "Convert .hs to .md for mdbook";
            };
          });

          tools = [
            # GHC should go before HLS - see https://github.com/NixOS/nixpkgs/issues/225895
            ghc
            hls
            ghcid
            cabal
            fourmolu
            hpack
          ];

          devShells.default = mkShell {
            packages = tools;
            bash.extra = "export LANG=C.utf8";
            commands =
              mkCommands "tools" tools
              ++ mkRunCommands "ide" { "codium ." = packages.codium; inherit (packages) writeSettings; }
              ++ mkRunCommands "docs" { inherit (packages) genDocs; };
          };
        in
        {
          inherit devShells packages toolsGHCPackage;
        }
        );
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
