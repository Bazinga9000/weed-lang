{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
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

      flake.nixosModules.default = { config, lib, pkgs, ... }: {
        options.services.weed-shds = with lib; {
          enable = mkEnableOption "the weed-shds Discord bot";

          package = mkOption {
            type = types.package;
            default = self.packages.${pkgs.stdenv.hostPlatform.system}.weed-shds;
            defaultText = lib.literalExpression "self.packages.<system>.weed-shds";
            description = "The weed-shds package to run.";
          };

          tokenPath = mkOption {
            type = types.path;
            description = ''
              Path to a file containing the Discord bot token.
            '';
          };
        };

        config = lib.mkIf config.services.weed-shds.enable {
          systemd.services.weed-shds = {
            description = "weed-shds Discord bot";
            wantedBy = [ "multi-user.target" ];
            after = [ "network-online.target" ];
            wants = [ "network-online.target" ];

            serviceConfig = {
              Type = "simple";
              ExecStart = "${lib.getExe config.services.weed-shds.package}";
              Environment = "TOKEN_FILE=${config.services.weed-shds.tokenPath}";
              Restart = "on-failure";
              RestartSec = 5;
            };
          };
        };
      };

      perSystem =
        { self', pkgs, lib, ... }:
        {
          haskellProjects.default = rec {
            basePackages = pkgs.haskell.packages.ghc910;

            packages = {
              discord-haskell.source =
                let
                  noOverride = lib.versionAtLeast basePackages.discord-haskell.version "1.19.0";
                in
                lib.warnIf noOverride
                  "discord-haskell >= 1.19.0 is now present in nixpkgs. This override can be removed."
                  (fetchTarball {
                    url = "https://hackage.haskell.org/package/discord-haskell-1.19.0/discord-haskell-1.19.0.tar.gz";
                    sha256 = "sha256:149makdvavapcvrrs5k40bzbyrk03w5xnhmyqnzzxjgzjd2bcn46";
                  });
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
