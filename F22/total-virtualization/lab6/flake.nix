{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    drv-tools.url = "github:deemp/flakes?dir=drv-tools";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    my-devshell.url = "github:deemp/flakes?dir=devshell";
    python-tools.url = "github:deemp/flakes?dir=language-tools/python";
  };
  outputs =
    { self
    , nixpkgs
    , drv-tools
    , flake-utils
    , my-devshell
    , python-tools
    , ...
    }: flake-utils.lib.eachDefaultSystem
      (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        devshell = my-devshell.devshell.${system};
        inherit (my-devshell.functions.${system}) mkCommands;
        inherit (drv-tools.functions.${system}) mkShellApps withAttrs;
        inherit (python-tools.snippets.${system}) activateVenv;
        tools = [
          (withAttrs pkgs.docker { name = "docker"; })
          (withAttrs pkgs.poetry { name = "poetry"; })
        ];
      in
      {
        devShells.default = devshell.mkShell
          {
            packages = tools;
            bash = {
              extra = activateVenv;
            };
            commands = mkCommands "tools" tools;
          };
      });

  nixConfig = {
    extra-trusted-substituters = [
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
