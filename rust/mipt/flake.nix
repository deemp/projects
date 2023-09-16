{
  inputs.flakes.url = "github:deemp/flakes";
  outputs = inputs: inputs.flakes.makeFlake {
    inputs = { inherit (inputs.flakes.all) nixpkgs codium devshell flakes-tools workflows rust-overlay; };
    perSystem = { inputs, system }:
      let
        pkgs = import inputs.nixpkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = [
            (import inputs.rust-overlay)
          ];
        };
        lib = pkgs.lib;
        inherit (pkgs.stdenv) isDarwin isLinux;

        rust_toolchain = {
          extensions = [ "rust-src" "rust-analysis" "rust-std" "rust-docs" "clippy" "rust-analyzer" "llvm-tools-preview" ];
        };

        inherit (inputs.codium.lib.${system}) mkCodium writeSettingsJSON extensions extensionsCommon settingsNix settingsCommonNix;
        inherit (inputs.devshell.lib.${system}) mkCommands mkRunCommands mkShell;

        packages = {
          # --- IDE ---

          # We compose `VSCodium` with extensions
          codium = mkCodium {
            # We use common extensions
            extensions = extensionsCommon // {
              # Next, we include the extensions from the pre-defined attrset
              inherit (extensions) rust;
            };
          };

          # a script to write `.vscode/settings.json`
          writeSettings = writeSettingsJSON (settingsCommonNix // { inherit (settingsNix) rust; });
        };

        tools = [
          pkgs.taplo
          (
            pkgs.rust-bin.nightly."2023-07-28".default.override rust_toolchain
          )
        ] ++ lib.lists.optionals isDarwin [
          pkgs.iconv
          pkgs.darwin.apple_sdk.frameworks.Security
        ];

        devShells.default = mkShell {
          packages = tools;
          bash.extra = ''
            export DYLD_FALLBACK_LIBRARY_PATH=$(rustc --print sysroot)/lib
          '' + (if
            isDarwin == true then ''
            export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${pkgs.iconv.out}/lib
          '' else '' '');
          commands =
            mkCommands "tools" tools
            ++ mkRunCommands "ide" { "codium ." = packages.codium; inherit (packages) writeSettings; };
        };
      in
      {
        inherit packages devShells;
      };
  };

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
