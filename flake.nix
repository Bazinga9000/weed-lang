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
              discord-haskell.source = fetchTarball {
                url = "https://hackage.haskell.org/package/discord-haskell-1.19.0/discord-haskell-1.19.0.tar.gz";
                sha256 = "sha256:149makdvavapcvrrs5k40bzbyrk03w5xnhmyqnzzxjgzjd2bcn46";
              };
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

          packages.weed-spec = pkgs.stdenvNoCC.mkDerivation {
            pname = "weed-spec";
            version = "0.1.0";
            src = ./spec;
            nativeBuildInputs = [ pkgs.typst ];

            buildPhase = ''
              export XDG_CACHE_HOME=$(mktemp -d)
              typst compile spec.typ
            '';

            installPhase = ''
              mkdir -p $out
              cp spec.pdf $out/
            '';
          };

          packages.default = self'.packages.weed-repl;
        };
    };
}
