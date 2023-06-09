{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    # update common after changes mainlined - https://github.com/nammayatri/common/pull/13
    common.url = "github:arjunkathuria/common/Mobility-GHC927-Rebased";
    flake-parts.follows = "common/flake-parts";
    haskell-flake.follows = "common/haskell-flake";

    # Haskell dependencies
    sequelize.url = "github:juspay/haskell-sequelize?rev=31948e744e431320492031cfddd01e7a9896631d";
    sequelize.flake = false;
    beam-mysql.url = "github:juspay/beam-mysql?rev=f8d029e6861ade3335a9f96b7be3b306d8208dd8";
    beam-mysql.flake = false;
    hedis.url = "github:juspay/hedis/ac9e247803379f65b3a5fc99771259134ac01129";
    hedis.flake = false;

    euler-events-hs.url = "github:juspay/euler-events-hs";
    euler-events-hs.inputs.haskell-flake.follows = "common/haskell-flake";

    juspay-extra.url = "github:juspay/euler-haskell-common";
    juspay-extra.inputs.haskell-flake.follows = "common/haskell-flake";

    word24.url = "github:winterland1989/word24";
    word24.flake = false;

    servant-mock.url = "github:arjunkathuria/servant-mock?rev=17e90cb831820a30b3215d4f164cf8268607891e";
    servant-mock.flake = false;

    tinylog.url = "git+https://gitlab.com/arjunkathuria/tinylog.git?rev=08d3b6066cd2f883e183b7cd01809d1711092d33";
    tinylog.flake = false;
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } ({ withSystem, ... }: {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.common.flakeModules.ghc927
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

          basePackages = config.haskellProjects.ghc927.outputs.finalPackages;

          packages = {
            beam-mysql.source = inputs.beam-mysql;
            hedis.source = inputs.hedis;
            sequelize.source = inputs.sequelize;
            word24.source = inputs.word24;
            servant-mock.source = inputs.servant-mock;
            tinylog.source = inputs.tinylog;
          };

          settings = {
            hedis = {
              check = false;
            };

            euler-events-hs = {
              jailbreak = true;
              check = false;
            };

            prometheus-client = {
              check = false;
            };

            prometheus-proc = {
              jailbreak = true;
            };

            word24 = {
              check = false;
            };

            servant-mock = {
              check = false;
              jailbreak=true;
            };
          };
        };
      };
    });
}
