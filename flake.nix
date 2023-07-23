{
  inputs.flakes.url = "github:deemp/flakes";
  outputs = inputs:
    let makeFlake = inputs.flakes.makeFlake; in
    makeFlake {
      inputs = {
        inherit (inputs.flakes.all) nixpkgs formatter drv-tools workflows flakes-tools devshell codium;
        inherit (inputs) flakes;
      };
      perSystem = { inputs, system }:
        let
          pkgs = inputs.nixpkgs.legacyPackages.${system};
          inherit (inputs.codium.lib.${system}) mkCodium writeSettingsJSON extensions extensionsCommon settingsNix settingsCommonNix;
          inherit (inputs.devshell.lib.${system}) mkCommands mkRunCommands mkShell;
          inherit (inputs.workflows.lib.${system}) writeWorkflow nixCI;
          inherit (inputs.flakes-tools.lib.${system}) mkFlakesTools;
          inherit (inputs.drv-tools.lib.${system}) mkShellApps subDirectories getExe;

          scripts = (mkShellApps {
            genDocs =
              {
                text = ''
                  (cd haskell && set -a && source .env && ${getExe (import ./haskell).packages.${system}.genDocs})
                  cp notes/README.md docs/src/miscNotes/README.md
                  (cd docs/src && cat prefix.md haskell/toc.md miscNotes/toc.md > SUMMARY.md)
                  ${getExe pkgs.mdbook} build docs
                '';
                description = "Generate docs";
              };
          });

          packages = {
            # --- IDE ---

            # VSCodium with extensions
            codium = mkCodium { extensions = extensionsCommon; };

            # a script to write `.vscode/settings.json`
            writeSettings = writeSettingsJSON settingsCommonNix;

            # --- Flakes ---

            # Scripts that can be used in CI
            inherit (mkFlakesTools
              {
                root = ./.;
                dirs = [
                  "blockchain"
                  "F22/total-virtualization"
                  "haskell"
                  "postgresql"
                  "prolog/maze"
                  "rescript"
                  "scala"
                  "vlang/vforces"
                  "."
                ];
                subDirs = [
                  "F22/total-virtualization"
                  "haskell"
                ];
              }) updateLocks saveFlakes format;

            # --- GH Actions

            # A script to write GitHub Actions workflow file into `.github/ci.yaml`
            writeWorkflows =
              import ./nix-files/workflow.nix {
                name = "ci";
                inherit scripts system;
                inherit (inputs) workflows;
              };
          } // scripts;

          tools = [
            pkgs.mdbook
          ];

          devShells.default = mkShell {
            packages = tools;
            commands =
              mkCommands "tools" tools
              ++ mkRunCommands "ide" { "codium ." = packages.codium; inherit (packages) writeSettings; }
              ++ mkRunCommands "docs" { inherit (packages) genDocs; }
              ++ mkRunCommands "infra" { inherit (packages) writeWorkflows; }
            ;
          };
        in
        {
          inherit packages devShells;
          formatter = inputs.formatter.${system};
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
