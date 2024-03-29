{
  outputs = inputs:
    let flakes = (import ../../.).outputs.inputs.flakes; in
    flakes.makeFlake {
      inputs = {
        inherit (flakes.all) nixpkgs drv-tools devshell;
        haskell = (import ../.);
      };
      perSystem = { inputs, system }:
        let
          pkgs = inputs.nixpkgs.legacyPackages.${system};
          inherit (inputs.devshell.lib.${system}) mkShell mkCommands;
          inherit (inputs.haskell.toolsGHCPackage.${system} "simplex-cheat" ./. { }) ghcid cabal hpack;
          tools = [ ghcid cabal hpack ];
          devShells.default = mkShell {
            packages = tools;
            bash.extra = "export LANG=C.utf8";
            commands =
              mkCommands "tools" tools
              ++ [
                {
                  name = "run";
                  command = "${pkgs.lib.getExe cabal} v1-run";
                  category = "run";
                  help = "run chat";
                }
                {
                  name = "write-config";
                  command = pkgs.lib.getExe (import nix-files/writers.nix {
                    inherit (inputs) drv-tools; inherit pkgs system;
                  });
                  category = "run";
                  help = "write config";
                }
              ];
          };
        in
        {
          inherit devShells;
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
