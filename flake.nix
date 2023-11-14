{
  inputs = {
    # Common is used only to get the GHC 9.2 package set.
    common.url = "github:nammayatri/common";

    nixpkgs.follows = "common/nixpkgs";
    flake-parts.follows = "common/flake-parts";
    haskell-flake.follows = "common/haskell-flake";

    cereal.url = "github:juspay/cereal";
    cereal.flake = false;

    juspay-extra.url = "github:juspay/euler-haskell-common";
    juspay-extra.inputs.haskell-flake.follows = "haskell-flake";

    euler-events-hs.url = "github:juspay/euler-events-hs/main";
    euler-events-hs.inputs.haskell-flake.follows = "haskell-flake";

    sequelize.url = "github:juspay/haskell-sequelize/beckn-compatible";
    sequelize.inputs.nixpkgs.follows = "nixpkgs";
    sequelize.inputs.haskell-flake.follows = "haskell-flake";
    sequelize.inputs.flake-parts.follows = "flake-parts";

    hedis.url = "git+https://github.com/juspay/hedis?rev=92a3d5ab73dcb0ea11139a01d6f2950a8b8e7e0e";
    hedis.flake = false;

    servant-mock.url = "github:arjunkathuria/servant-mock?rev=17e90cb831820a30b3215d4f164cf8268607891e";
    servant-mock.flake = false;

    tinylog.url = "git+https://gitlab.com/arjunkathuria/tinylog.git?rev=08d3b6066cd2f883e183b7cd01809d1711092d33";
    tinylog.flake = false;
  };

  outputs = inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
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
            inputs.sequelize.haskellFlakeProjectModules.output
          ];
          basePackages = config.haskellProjects.ghc927.outputs.finalPackages;
          packages = {
            hedis.source = inputs.hedis;
            cereal.source = inputs.cereal;
            servant-mock.source = inputs.servant-mock;
            tinylog.source = inputs.tinylog;
          };
          settings = {
            bytestring-conversion.broken = false;
            hedis.check = false;
            sequelize.check = false;
            servant-client = {
              jailbreak = true;
            };
            servant-client-core = {
              jailbreak = true;
            };
            servant-server = {
              jailbreak = true;
            };
            servant-mock = {
              check = false;
              jailbreak = true;
            };
            servant = {
              jailbreak = true;
            };
          };
          autoWire = [ "packages" "checks" "devShells" "apps"];
        };
      };
    };
}
