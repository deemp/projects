{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    my-codium.url = "github:deemp/flakes?dir=codium";
    flakes-tools.url = "github:deemp/flakes?dir=flakes-tools";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    vscode-extensions_.url = "github:deemp/flakes?dir=source-flake/vscode-extensions";
    vscode-extensions.follows = "vscode-extensions_/vscode-extensions";
    my-devshell.url = "github:deemp/flakes?dir=devshell";
    python-tools.url = "github:deemp/flakes?dir=language-tools/python";
  };
  outputs =
    { self
    , nixpkgs
    , my-codium
    , flake-utils
    , vscode-extensions
    , my-devshell
    , python-tools
    , flakes-tools
    , ...
    }: flake-utils.lib.eachDefaultSystem
      (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        inherit (my-codium.functions.${system}) mkCodium writeSettingsJSON;
        inherit (my-codium.configs.${system}) extensions settingsNix;
        inherit (vscode-extensions.packages.${system}) vscode open-vsx;
        inherit (flakes-tools.functions.${system}) mkFlakesTools;
        createVenvs = python-tools.functions.${system}.createVenvs [ "lab5" "lab6" ];
        writeSettings = writeSettingsJSON {
          inherit (settingsNix) todo-tree files editor gitlens
            git nix-ide workbench markdown-all-in-one python
            markdown-language-features yaml kubernetes
            ;
        };
        codiumTools = builtins.attrValues {
          inherit (pkgs) rabbitmq-server hadolint poetry;
          inherit writeSettings createVenvs;
        };
        codium = mkCodium {
          extensions = {
            inherit (extensions) 
              nix misc markdown github docker 
              python toml yaml kubernetes;
          };
          runtimeDependencies = codiumTools;
        };
        devshell = my-devshell.devshell.${system};
        inherit (my-devshell.functions.${system}) mkCommands;
        flakesTools = mkFlakesTools [ "lab4" "lab5" "lab6" "lab7" "." ];
        tools = codiumTools ++ [ codium ];
      in
      {
        packages = {
          inherit (flakesTools) updateLocks pushToCachix;
        };
        devShells.default = devshell.mkShell
          {
            packages = tools;
            bash.extra = '''';
            commands = (mkCommands "ide" tools);
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
