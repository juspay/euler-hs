{
  inputs = {
    common.url = "github:juspay/nix-common";
    # Euler packages
    euler-events-hs = {
      url = "github:juspay/euler-events-hs";
      inputs.common.follows = "common";
    };
    euler-haskell-common = {
      url = "github:juspay/euler-haskell-common";
      inputs.common.follows = "common";
    };
    haskell-sequelize = {
      url = "github:juspay/haskell-sequelize";
      inputs.common.follows = "common";
    };

    resource-pool = {
      type = "git";
      url = "https://github.com/juspay/pool";
      ref = "ghc-9.2.8";
      rev = "581813890b289de5060ddcd04f3822ae0085567b";
      flake = false;
    };
  };
  outputs = inputs:
    inputs.common.lib.mkFlake { inherit inputs; } {
      perSystem = { config, self', pkgs, pkgs-latest, ... }: {

        haskellProjects.default = let fs = pkgs-latest.lib.fileset; in {
          projectRoot = builtins.toString (fs.toSource {
            root = ./.;
            fileset = fs.unions [
              ./src
              ./test
              ./testDB
              ./euler-hs.cabal
              ./.juspay
            ];
          });
          imports = [
            inputs.euler-events-hs.haskellFlakeProjectModules.output
            inputs.euler-haskell-common.haskellFlakeProjectModules.output
            inputs.haskell-sequelize.haskellFlakeProjectModules.output
          ];

          autoWire = [ "packages" ];

          packages = {
            # If your package is a dependency for all euler packages, add it to
            # `euler-nix-common` instead.
            resource-pool.source = inputs.resource-pool;
          };
          settings = {
            # FIXME: make the tests work before merging
            euler-hs = {
              check = false;
              buildAnalysis = true;
            };
            fieldInspector.cabalFlags.enable-api-contract-plugins = true;
          };
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.euler-hs;

        devShells.default = pkgs.mkShell {
          name = "euler-hs";
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.devShells.common
          ];
        };
      };
    };
}
