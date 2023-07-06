{
  inputs = { };

  outputs = inputs:
    let
      inputs_ =
        let haskell = (import ../.); in
        {
          inherit (haskell.outputs.inputs) devshell nixpkgs flake-utils codium;
          inherit haskell;
        };

      outputs = outputs_ { } // { inputs = inputs_; outputs = outputs_; };

      outputs_ =
        inputs__:
        let inputs = inputs_ // inputs__; in
        inputs.flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = inputs.nixpkgs.legacyPackages.${system};
          inherit (inputs.devshell.lib.${system}) mkShell mkCommands mkRunCommands mkShellCommands;
          inherit (inputs.codium.lib.${system}) mkCodium writeSettingsJSON settingsCommonNix settingsNix extensionsCommon extensions;
          inherit (inputs.haskell.toolsGHCPackage.${system} "try-esqueleto" ./. { }) hls ghcid cabal fourmolu hpack;

          haskell-tools = [ hls ghcid cabal fourmolu hpack ];
          infra-tools = [ pkgs.kubectl pkgs.postgresql_15 pkgs.helm ];

          packages = {
            # --- IDE ---
            codium = mkCodium { extensions = extensionsCommon // { inherit (extensions) haskell kubernetes postgresql; }; };

            # a script to write `.vscode/settings.json`
            writeSettings = writeSettingsJSON (settingsCommonNix // { inherit (settingsNix) haskell; });
          };

          devShells.default = mkShell {
            packages = haskell-tools ++ infra-tools;
            bash.extra = "export LANG=C.utf8";
            commands =
              mkCommands "haskell" haskell-tools
              ++ mkCommands "infra" infra-tools
              ++ mkRunCommands "ide" { "codium ." = packages.codium; inherit (packages) writeSettings; };
          };
        in
        {
          inherit devShells;
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
