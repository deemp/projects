{
  inputs = { };

  outputs = inputs:
    let
      inputs_ =
        let flakes = (import ../..).outputs.inputs.flakes; in
        {
          inherit (flakes.source-flake) nixpkgs flake-utils;
          inherit (flakes) devshell codium drv-tools;
        };

      outputs = outputs_ { } // { inputs = inputs_; outputs = outputs_; };

      outputs_ =
        inputs__:
        let inputs = inputs_ // inputs__; in
        inputs.flake-utils.lib.eachDefaultSystem
          (system:
          let
            pkgs = inputs.nixpkgs.legacyPackages.${system};
            inherit (inputs.codium.lib.${system}) mkCodium writeSettingsJSON;
            inherit (inputs.codium.lib.${system}) extensions extensionsCommon settingsCommonNix settingsNix;
            inherit (inputs.devshell.lib.${system}) mkCommands mkRunCommands mkShell;
            inherit (inputs.drv-tools.lib.${system}) mkShellApps getExe;

            packages = {
              # --- IDE ---

              # This part can be removed if you don't use `VSCodium`
              # We compose `VSCodium` with dev tools
              # This is to let `VSCodium` run on its own, outside of a devshell
              codium = mkCodium {
                extensions = extensionsCommon // { inherit (extensions) prolog pdf; };
              };

              # a script to write `.vscode/settings.json`
              writeSettings = writeSettingsJSON settingsCommonNix;
            } // (mkShellApps {
              run = {
                text = ''${getExe pkgs.swiProlog} -s main.pl -g 'run.' -t 'halt.'; printf "\n\n"'';
                description = "Run game script";
              };
            });

            tools = [ pkgs.swiProlog ];

            devShells.default = mkShell {
              packages = tools ++ [ packages.run ];
              bash.extra = "";
              commands =
                mkCommands "tools" tools
                ++ mkRunCommands "ide" { "codium ." = packages.codium; inherit (packages) writeSettings; }
                ++ mkCommands "run" [ packages.run ]
              ;
            };
          in
          {
            inherit packages devShells;
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
