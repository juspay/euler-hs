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
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.common.flakeModules.ghc810
        inputs.haskell-flake.flakeModule
      ];
      perSystem = { self', pkgs, lib, config, ... }: {
        packages.default = self'.packages.euler-hs;
        haskellProjects.default = {
          imports = [
            self.haskellFlakeProjectModules.allOverrides
          ];
          basePackages = config.haskellProjects.ghc810.outputs.finalPackages;
        };
      };
      flake.haskellFlakeProjectModules.allOverrides = { pkgs, lib, ... }: {
        source-overrides = {
          sequelize = inputs.sequelize;
          beam-core = inputs.beam + /beam-core;
          beam-migrate = inputs.beam + /beam-migrate;
          beam-postgres = inputs.beam + /beam-postgres;
          beam-sqlite = inputs.beam + /beam-sqlite;
          beam-mysql = inputs.beam-mysql;
          mysql-haskell = inputs.mysql-haskell;
          hedis = inputs.hedis;
        };
        overrides =
          let
            # A function that enables us to write `foo = [ dontCheck ]` instead of `foo =
            # lib.pipe super.foo [ dontCheck ]` in haskell-flake's `overrides`.
            compilePipe = f: self: super:
              lib.mapAttrs
                (name: value:
                  if lib.isList value then
                    lib.pipe super.${name} value
                  else
                    value
                )
                (f self super);
          in
          compilePipe (self: super: with pkgs.haskell.lib.compose; {
            sequelize = [ dontCheck ];

            beam-core = [ doJailbreak ];
            beam-migrate = [ doJailbreak ];
            beam-mysql = [ dontCheck doJailbreak ];
            beam-postgres = [ dontCheck doJailbreak ];
            beam-sqlite = [ dontCheck doJailbreak ];
            mysql-haskell = [ dontCheck doJailbreak ];
            hedis = [ dontCheck ];
          });
      };
    };
}
