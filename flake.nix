{
  inputs.flakes.url = "github:deemp/flakes";
  outputs = inputs:
    let
      inputs_ =
        let flakes = inputs.flakes.flakes; in
        {
          inherit (flakes.source-flake) nixpkgs flake-utils formatter;
          inherit (flakes) drv-tools workflows flakes-tools devshell codium;
          inherit flakes;
        };

      outputs = flake { } // { inherit flake; inputs = inputs_; };

      flake =
        inputs__:
        let inputs = inputs_ // inputs__; in
        inputs.flake-utils.lib.eachDefaultSystem
          (system:
          let
            pkgs = inputs.nixpkgs.legacyPackages.${system};
            inherit (inputs.codium.lib.${system}) mkCodium writeSettingsJSON;
            inherit (inputs.codium.lib.${system}) extensions extensionsCommon settingsNix settingsCommonNix;
            inherit (inputs.devshell.lib.${system}) mkCommands mkRunCommands mkShell;
            inherit (inputs.workflows.lib.${system}) writeWorkflow;
            inherit (inputs.workflows.lib.${system}) nixCI;
            inherit (inputs.flakes-tools.lib.${system}) mkFlakesTools;
            inherit (inputs.drv-tools.lib.${system}) mkShellApps subDirectories getExe;

            scripts = (mkShellApps {
              genDocs =
                {
                  text = ''
                    (cd haskell && ${getExe (import ./haskell).packages.${system}.genDocs})
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
              inherit (mkFlakesTools [
                "blockchain"
                "F22/total-virtualization"
                (subDirectories ./. "F22/total-virtualization")
                "haskell"
                (subDirectories ./. "haskell")
                "postgresql"
                "prolog/maze"
                "rescript"
                "scala"
                "vlang/vforces"
                "."
              ]) updateLocks pushToCachix format;

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
          });
    in
    outputs;

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
