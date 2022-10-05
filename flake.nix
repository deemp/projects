{
  inputs = {
    nixpkgs_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/nixpkgs;
    flake-utils_.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/flake-utils;    
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils.follows = "flake-utils_/flake-utils";
    
    my-formatter.url = github:br4ch1st0chr0n3/flakes?dir=source-flake/formatter;
    my-codium.url = github:br4ch1st0chr0n3/flakes?dir=codium;
    # my-codium.url = path:./codium;
  };
  outputs =
    { self
    , nixpkgs
    , flake-utils
    , my-codium
    , my-formatter
    }: flake-utils.lib.eachDefaultSystem
      (system:
      let
        inherit (my-codium.tools.${system})
          extensions
          toList
          shellTools
          mkCodium
          mkDevShellsWithDefault
          mkFlakesUtils
          mkShellApp
          flakesToggleRelativePaths
          ;
        pkgs = nixpkgs.legacyPackages.${system};

        flakesUtils = (mkFlakesUtils [ "source-flake" "codium" "env2json" "json2md" "inputs" "." ]);

        toggleRelativePaths_ =
          let
            myCodium = "my-codium";
            toggleConfig = [
              { "." = [ myCodium ]; }
            ];
          in
          flakesToggleRelativePaths toggleConfig flakesUtils.flakesUpdate;

        codium = mkCodium {
          extensions = { inherit (extensions) nix misc github fish; };
          runtimeDependencies = [
            (toList { inherit (shellTools) nix docker; })
            toggleRelativePaths_
            (builtins.attrValues flakesUtils)
            pkgs.inotify-tools
          ];
        };
      in
      {

        devShells = mkDevShellsWithDefault
          {
            buildInputs = [
              (builtins.attrValues flakesUtils)
              toggleRelativePaths_
            ];
          }
          {
            enter = { buildInputs = [ pkgs.gawk ]; };
          };
        packages = {
          pushToCachix = flakesUtils.flakesPushToCachix;
          updateLocks = flakesUtils.flakesUpdate;
          format = flakesUtils.flakesFormat;
          default = codium;
        };
      }) // { inherit (my-formatter) formatter; };

  nixConfig = {
    extra-trusted-substituters = [
      https://haskell-language-server.cachix.org
      https://nix-community.cachix.org
      https://hydra.iohk.io
      https://br4ch1st0chr0n3.cachix.org
    ];
    extra-trusted-public-keys = [
      haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8=
      nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=
      hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
      br4ch1st0chr0n3.cachix.org-1:o1FA93L5vL4LWi+jk2ECFk1L1rDlMoTH21R1FHtSKaU=
    ];
  };
}
