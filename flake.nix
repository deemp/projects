{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    codium.url = "github:deemp/flakes?dir=codium";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    devshell.url = "github:deemp/flakes?dir=devshell";
    flakes-tools.url = "github:deemp/flakes?dir=flakes-tools";
    drv-tools.url = "github:deemp/flakes?dir=drv-tools";
    workflows.url = "github:deemp/flakes?dir=workflows";
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem
    (system:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        inherit (inputs.codium.lib.${system}) mkCodium writeSettingsJSON;
        inherit (inputs.codium.lib.${system}) extensions extensionsCommon settingsNix settingsCommonNix;
        inherit (inputs.devshell.lib.${system}) mkCommands mkRunCommands mkShell;
        inherit (inputs.workflows.lib.${system}) writeWorkflow;
        inherit (inputs.workflows.lib.${system}) nixCI;
        inherit (inputs.flakes-tools.lib.${system}) mkFlakesTools;
        inherit (inputs.drv-tools.lib.${system}) mkShellApps;

        scripts = (mkShellApps {
          genDocs = {
            text = ''
              (cd hs-notes && ${pkgs.nix}/bin/nix run .#genDocs)
              ${pkgs.mdbook}/bin/mdbook build docs
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
          inherit (mkFlakesTools [
            "blockchain"
            "db-hs"
            "developers-roadmap"
            "scala"
            "simplex-cheat"
            "sockets-and-pipes"
            "manager"
            "manager/nix-dev"
            "optics-by-example"
            "ts-serializable-test"
            "vforces"
            "."
          ]) updateLocks pushToCachix;

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
            mkCommands "tools" tools ++
            mkRunCommands "ide" {
              "codium ." = packages.codium;
              inherit (packages) writeSettings;
            } ++
            mkRunCommands "docs" {
              inherit (packages) genDocs;
            }
          ;
        };
      in
      {
        inherit packages devShells;
      });

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
