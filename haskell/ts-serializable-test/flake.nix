{
  inputs = { };
  outputs = inputs:
    let flakes = (import ../../.).outputs.inputs.flakes; in
    flakes.makeFlake {
      inputs = {
        inherit (flakes.all) nixpkgs codium drv-tools devshell;
        haskell = (import ../.);
      };
      perSystem = { inputs, system }:
        let
          pkgs = inputs.nixpkgs.legacyPackages.${system};
          inherit (inputs.devshell.lib.${system}) mkShell mkCommands mkRunCommands mkShellCommands;
          inherit (inputs.codium.lib.${system}) mkCodium writeSettingsJSON settingsCommonNix settingsNix extensionsCommon extensions;
          inherit (inputs.haskell.toolsGHCPackage.${system} "ts-serializable-test" ./. { }) hls ghcid cabal fourmolu hpack;

          haskell-tools = [ hls ghcid cabal fourmolu hpack ];
          node-tools = [ pkgs.nodejs_18 ];

          packages = {
            # --- IDE ---
            codium = mkCodium { extensions = extensionsCommon // { inherit (extensions) haskell kubernetes postgresql; }; };

            # a script to write `.vscode/settings.json`
            writeSettings = writeSettingsJSON (settingsCommonNix // { inherit (settingsNix) haskell; });
          };

          devShells.default = mkShell {
            packages = haskell-tools ++ node-tools;
            bash.extra = "export LANG=C.utf8";
            commands =
              mkCommands "haskell" haskell-tools
              ++ mkCommands "node" node-tools
              ++ mkRunCommands "ide" { "codium ." = packages.codium; inherit (packages) writeSettings; };
          };
        in
        {
          inherit packages devShells;
        };
    };

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
