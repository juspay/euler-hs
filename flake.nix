{
  inputs = {
    # Common is used only to get the GHC 8.10 package set.
    common.url = "github:nammayatri/common";
    nixpkgs.follows = "common/nixpkgs";
    flake-parts.follows = "common/flake-parts";
    haskell-flake.follows = "common/haskell-flake";

    # Haskell dependencies
    cereal.url = "github:juspay/cereal/213f145ccbd99e630ee832d2f5b22894c810d3cc";
    cereal.flake = false;

    juspay-extra.url = "github:juspay/euler-haskell-common";
    juspay-extra.inputs.haskell-flake.follows = "common/haskell-flake";

    euler-events-hs.url = "github:juspay/euler-events-hs/74ba3c12e94b37a0dc5a44c5a64d6d3da8cb0f67";
    euler-events-hs.inputs.haskell-flake.follows = "common/haskell-flake";

    sequelize.url = "github:juspay/haskell-sequelize/beckn-compatible";
    sequelize.flake = false;

    beam.url = "github:srid/beam/ghc810"; 
    beam.flake = false;

    beam-mysql.url = "github:juspay/beam-mysql/4c876ea2eae60bf3402d6f5c1ecb60a386fe3ace";
    beam-mysql.flake = false;

    mysql-haskell.url = "github:juspay/mysql-haskell/788022d65538db422b02ecc0be138b862d2e5cee"; 
    mysql-haskell.flake = false;

    hedis.url = "github:juspay/hedis/22d814672d8476a6f8fb43047af2897afbf77ac6";
    hedis.flake = false;
  };
  outputs = inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.common.flakeModules.ghc810
        inputs.haskell-flake.flakeModule
      ];
      perSystem = { self', pkgs, lib, config, ... }: {
        packages.default = self'.packages.euler-hs;
        haskellProjects.default = {
          projectFlakeName = "euler-hs";
          imports = [
            inputs.euler-events-hs.haskellFlakeProjectModules.output
            inputs.juspay-extra.haskellFlakeProjectModules.output
          ];
          basePackages = config.haskellProjects.ghc810.outputs.finalPackages;
          packages = {
            beam-core.source = inputs.beam + /beam-core;
            beam-migrate.source = inputs.beam + /beam-migrate;
            beam-mysql.source = inputs.beam-mysql;
            beam-postgres.source = inputs.beam + /beam-postgres;
            beam-sqlite.source = inputs.beam + /beam-sqlite;
            hedis.source = inputs.hedis;
            mysql-haskell.source = inputs.mysql-haskell;
            sequelize.source = inputs.sequelize;
            cereal.source = inputs.cereal;
          };
          settings = {
            beam-core.jailbreak = true;
            beam-migrate.jailbreak = true;
            beam-mysql = {
              check = false;
              jailbreak = true;
            };
            beam-postgres = {
              check = false;
              jailbreak = true;
            };
            beam-sqlite = {
              check = false;
              jailbreak = true;
            };
            hedis.check = false;
            mysql-haskell = {
              check = false;
              jailbreak = true;
            };
            sequelize.check = false;
            cereal = {
              check = false;
              jailbreak = true;
            };
            euler-events-hs = {
              check = false;
              jailbreak = true;
            };
            juspay-extra = {
              check = false;
              jailbreak = true;
            };
            nonempty-containers = {
              jailbreak = true;
            };
            servant-client = {
              jailbreak = true;
            };
            servant-client-core = {
              jailbreak = true;
            };
            servant-server = {
              jailbreak = true;
            };
          };
        };
      };
    };
}



