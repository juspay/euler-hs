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

    euler-events-hs.url = "github:juspay/euler-events-hs/main";
    euler-events-hs.inputs.haskell-flake.follows = "common/haskell-flake";

    sequelize.url = "github:juspay/haskell-sequelize/dc01b0f9e6ba5a51dd8f9d0744a549dc9c0ba244";
    sequelize.flake = false;

    beam.url = "github:srid/beam/ghc810"; 
    beam.flake = false;

    beam-mysql.url = "github:juspay/beam-mysql/b4dbc91276f6a8b5356633492f89bdac34ccd9a1";
    beam-mysql.flake = false;

    mysql-haskell.url = "github:juspay/mysql-haskell/788022d65538db422b02ecc0be138b862d2e5cee"; # https://github.com/winterland1989/mysql-haskell/pull/38
    mysql-haskell.flake = false;
    hedis.url = "git+https://github.com/juspay/hedis?rev=92a3d5ab73dcb0ea11139a01d6f2950a8b8e7e0e";
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