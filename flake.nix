{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-parts,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem =
        { self', pkgs, ... }:
        {
          haskellProjects.default = {
            basePackages = pkgs.haskell.packages.ghc910;

            packages = {
            };
            settings = {
            };
            devShell = {
              enable = true;

              tools = hp: {
                alex = hp.alex;
                happy = hp.happy;
                fourmolu = hp.fourmolu;
                cabal-install = hp.cabal-install;
                haskell-language-server = hp.haskell-language-server;
                hlint = hp.hlint;
                ghcid = hp.ghcid;
              };
            };
          };

          packages.default = self'.packages.weed-lang;
        };
    };
}
