{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    common.url = "github:nammayatri/common";
    flake-parts.follows = "common/flake-parts";
    haskell-flake.follows = "common/haskell-flake";

    # Haskell dependencies
    sequelize.url = "github:juspay/haskell-sequelize/3abc8fe10edde3fd1c9a776ede81d057dc590341";
    sequelize.flake = false;
    beam.url = "github:srid/beam/ghc810";
    beam.flake = false;
    beam-mysql.url = "github:juspay/beam-mysql/4c876ea2eae60bf3402d6f5c1ecb60a386fe3ace";
    beam-mysql.flake = false;
    mysql-haskell.url = "github:juspay/mysql-haskell/788022d65538db422b02ecc0be138b862d2e5cee"; # https://github.com/winterland1989/mysql-haskell/pull/38
    mysql-haskell.flake = false;
    hedis.url = "github:juspay/hedis/46ea0ea78e6d8d1a2b1a66e6f08078a37864ad80";
    hedis.flake = false;
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } ({ withSystem, ... }: {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.common.flakeModules.ghc810
        inputs.haskell-flake.flakeModule
      ];
      perSystem = { self', pkgs, lib, config, ... }: {
        packages.default = self'.packages.euler-hs;
        haskellProjects.default = {
          basePackages = config.haskellProjects.ghc810.outputs.finalPackages;
          source-overrides = {
            beam-core = inputs.beam + /beam-core;
            beam-migrate = inputs.beam + /beam-migrate;
            beam-mysql = inputs.beam-mysql;
            beam-postgres = inputs.beam + /beam-postgres;
            beam-sqlite = inputs.beam + /beam-sqlite;
            hedis = inputs.hedis;
            mysql-haskell = inputs.mysql-haskell;
            sequelize = inputs.sequelize;
          };
          overrides = self: super:
            with pkgs.haskell.lib.compose;
            lib.mapAttrs (k: v: lib.pipe super.${k} v) {
              beam-core = [ doJailbreak ];
              beam-migrate = [ doJailbreak ];
              beam-mysql = [ dontCheck doJailbreak ];
              beam-postgres = [ dontCheck doJailbreak ];
              beam-sqlite = [ dontCheck doJailbreak ];
              hedis = [ dontCheck ];
              mysql-haskell = [ dontCheck doJailbreak ];
              sequelize = [ dontCheck ];
            };
        };
      };
    });
}
