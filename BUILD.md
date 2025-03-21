# Build with nix
This section was last updated at: 2020-06-16
## eulerBuild
`eulerBuild` is a collection of nix functions that builds on top of nixpkgs overlays and nixpkgs Haskell infrastructure.
The main benefit is composability of overrides and that you can define them in separate places then fetch and import in another package.
The guiding principle was about avoiding extra rebuilds and reusing nixpkgs binary cache as much as possible (hence there are some caveats, see the "important notes" section).

For more info on that you can refer to:

  - https://nixos.wiki/wiki/Overlays
  - https://nixos.org/nixpkgs/manual/#chap-overlays
  - https://nixos.org/nixpkgs/manual/#haskell

You can also read up on the implementation of the `fix` function in nixpkgs.

We define euler-related and euler-hs-using packages inside a new nixpkgs attribute `eulerHaskellPackages`.

The next "exported ovelays" section is mostly to be in the upper part of the screen for quicker reference and less scrolling.
Skip it if you don't need that info and want to read about how to use `eulerBuild`.

### Exported overlays in euler libraries
The following repos contain overrides for `eulerHaskellPackages` (and `haskellPackages`).
#### `euler-hs`
- `beam-overlay`
    Contains overrides for using our forks of `beam-*` libraries and build fixes for them.
    Also see an important note about these overrides in the next section.
- `code-tools-overlay`
   Build fixes for formatters, linter, etc. Currently only has an overide for `cabal-fmt`.
   This is for tools used in CI and development.
- `devtools-overlay`
   Build fixes for development tools only (`ghcid`, `ghcide`, etc.). Currently is empty.
- `euler-hs-overlay`
   Adds the `euler-hs` library.

`euler-hs` also exports a composition of `code-tools`, `beam` and `euler-hs` overlays in the `overlay` attribute.
#### `euler-db`
- `euler-db-overlay`
  Adds the `euler-db` library.

This library depends on `euler-hs` only because it needs `eulerBuild`. The Haskell code does not depend on `euler-hs`.

`euler-db` also exports a composition of `code-tools`, `beam` and `euler-db` overlays in the `overlay` attribute.
#### `euler-types`
- `euler-types-overlay`
  Adds the `euler-types` library.
  Contains a lot of overrides for fixing builds of `currency-codes`, `hedgehog` classes.
  Overrides `generic-lens`, overrides `optics-core` to use version 0.3.

`euler-types` also exports a composition of `euler-db.overlay` and `euler-types` overlays in the `overlay` attribute.
#### `euler-webservice`
- `euler-webservice-overlay`
  Adds the `euler-webservice` library.
  Overrides `generic-lens`.

`euler-webservice` also exports a composition of `euler-hs.overlay` and `euler-webservice` overlays in the `overlay` attribute.

### Overview of `eulerBuild`
`eulerBuild` contains:

  1. reexport of `fetchFromGitHub` from nixpkgs
  2. `importOverlay`
  3. `composeOverlays` which takes a list of overlays and returns a composed overlay.
    Overlays are compose from left to right, so later elements override previous ones in case of name clashes.
  4. `mkHaskellOverlay` which is used for defining overrides for `haskellPackages`.
      This is mostly used in external tools (e.g. `cabal-fmt`) to avoid extra rebuilds and use more of the built derivations from the binary cache.
  5. `mkEulerHaskellOverlay` which is used for defining overrides for `eulerHaskellPackages`.
    This is a main euler overlay creation function with which you should define an overlay for your package that has a need for `eulerHaskellPackages`.
  6. `allowedPaths` (described in later sections)
  7. `fastBuild` (described in later sections)
  8. `fastBuildExternal` (described in later sections)
  9. `mkShell` (described in later sections)

It can receive the following params:

1. `nixpkgs` (used for getting `fetchFromGitHub` and `lib`).
2. `haskellCompiler` a ghc package that should be used.
    We mostly rely on a default attribute from `default.nix` from packages that use `eulerBuild`.
3. `fastBuildParams` (described in later sections)

You can override params via the `.override { ... }` method.

### File structure
We currently use the following file structure for our libraries and projects using `eulerBuild`:

- `./default.nix`
- `./shell.nix`
- `./nix` -- contains all nix-related files
  * `/overlays` -- contains definitions of overlays
  * `/packages` -- contains `.nix` files and patches for some of the packages.
    It is preferable to use `callCabal2nix`, but sometimes it gives you an infinite recursion or fail in another way. In that case you can clone the repo of the package and run `cabal2nix .` manually and then add it to the `./nix/packages` folder.

This currently doesn't look very convenient for a single-overlay repos.
This can be changed, waiting for any feedback.

### Specifying allowed paths
To avoid extra rebuilds and polluting nix store and memory with unrelated files you need to explicitly specify what paths should be used accessible during the build.
This is built on top of https://github.com/manveru/nix-inclusive library.

Generally you should only include cabal files and folders with sources, tests, benchmarks, etc, like `src`, `app`, `test`, `bench`.
An example:
```nix
  euler-hs-src = eulerBuild.allowedPaths {
    root =  ./.;
    paths = [
      ./euler-hs.cabal
      ./src
      ./test
      ./testDB
    ];
  };
```

If you need something else to be used during the build process (which can include running tests for example; it depends on how you configure it) you should also add it to the allowed paths.
Here's an example from the `euler-api-gateway` which uses fixtures from the `./fixtures` folder for tests:
```nix
  euler-api-gateway-src = eulerBuild.allowedPaths {
    root = ./.;
    paths = [
      ./euler-api-gateway.cabal
      ./app
      ./src
      ./test
      ./fixtures
    ];
  };
```

### Important note about certain overrides
`beam-migrate` library depends on a `haskell-src-exts` version 1.21 and does not build with newer versions.
Instead of overriding `haskell-src-exts` attribute we pass it to the `beam-migrate` arguments:
```nix
beam-migrate =
  eulerBuild.fastBuildExternal {
    drv = hself.callCabal2nix "beam-migrate" beam-migrate-path {
      haskell-src-exts = haskell-src-exts_1_21_1;
    };
  };
```

This is done because a lot of packages depend on `haskell-src-exts` and we'll have a lot of rebuilds this way.

If there are packages that use `haskell-src-exts` directly you should also override their params like this:
```nix
haskell-src-meta =
  eulerBuild.fastBuildExternal {
    drv = hsuper.haskell-src-meta.override {
      haskell-src-exts = haskell-src-exts_1_21_1;
    };
  };
```

Without that cabal will fail with a mismatched dependencies error -- `beam-migrate` wants a 1.21 version of `haskell-src-exts`, but for example `haskell-src-meta` wants a 1.22 version.

This can be a pain to debug, so we'll include a script which looks for dependencies which depend on `haskell-src-exts` via the `nix why-depends` command.

### Defining an eulerBuild overlay
Add a new `.nix` file in the `./nix/overlays/` folder (the path is by convention, for the internal implementation it's not important where you put your files).
And use:

- `mkHaskellOverlay` which is used for defining overrides for `haskellPackages`.
    This is mostly used in external tools (e.g. `cabal-fmt`) to avoid extra rebuilds and use more of the built derivations from the binary cache.
- `mkEulerHaskellOverlay` which is used for defining overrides for `eulerHaskellPackages`.
   This is a main euler overlay creation function with which you should define an overlay for your package that has a need for `eulerHaskellPackages`.
   
When using euler libraries you'll mostly use `mkEulerHaskellOverlay`.
Let's look at a `euler-hs` overlay definition:
```nix
{
  eulerBuild # all eulerBuild-based overlays should take an eulerBuild parameter
, src # we add this parameter because we want to pass path to sources of the project we are in
}:
let
  # fetch a hedis repo via the reexport of fetchFromGitHub
  hedis-repo = eulerBuild.fetchFromGitHub {
    owner = "juspay";
    repo = "hedis";
    rev = "4ea54f16c0057acc99a9f0e9b63ea51ea4bf420e";
    sha256 = "094r4pxkc3h6w2vy4lha1zfdz29qihvkx2wi3mb7g1m3a6c7xp4h";
  };

  hedis-path = hedis-repo;
in
# call to mkEulerOverlay
# an argument should be a function taking 4 args
# 1. self -- nixpkgs overlay argument; nixpkgs set "from the future"
# 2. super -- nixpkgs overlay argument; "previous" nixpkgs set
# 3. hself -- "a resulting" set of haskell packages
# 4. hsuper -- "a previous" set of haskell packages
# If this doesn't make sense read about overlays and the fixed-point function in nix
# TODO: add docs for it here
#
# The names don't matter for the implementation, this is mostly for convention.
# Names are also up for debate (e.g. some use OldPackages: newPackages: and etc.)
eulerBuild.mkEulerHaskellOverlay
  (self: super: hself: hsuper: {
    hedis =
      eulerBuild.fastBuildExternal {
        drv = hself.callCabal2nix "hedis" hedis-path { };
      };
      
    # this is to show that calls to fastBuild are not a requirement
    test =
      hsuper.test;

    euler-hs =
      eulerBuild.fastBuild {
        # specify a derivation that we want to "fastBuil"
        # (see the next section for explanation)
        drv = hself.callCabal2nix "euler-hs" src { };
        
        # This is used for CI builds, so we want to run tests
        overrides = {
          runTests = true;
        };
      };
  })
```

It's better to avoid `rec {}` when defining overlays and use `let ... in` or define and compose new overlays to avoid one potential source of infite recursion bugs.

What if you want to define overrides for some other attributes (not haskell packages)?
You can use a regular nixpkgs overlay (`self: super: ...`) and import it via `import ./nix/overlays/myoverlay.nix`.
You can add any parameters or other things to it as it's just a function.
Overrides defined without `mk*Overlay` functions will also compose with `mk*Overlay` versions.
If you want to do nested override (like we do with haskell packages) you can look at the implementation of `mkHaskellOverlayWith` in `euler-hs/nix/euler-build/default.nix` for an example.

### Importing and using overlays
So, you have defined an overlay -- how to start using it?
Use
```nix
myOverlay = eulerBuild.importOverlay ./nix/overlay/myoverlay.nix {
    args = that;
    we = need;
    to = pass;
    to_the = overlay;
  }
```

Once we've imported overlay we can use it for customising nixpkgs:
```nix
pkgs = import nixpkgs {
  overlays = [ myOverlay1 myOverlay2 ];
}
```

If you are writing a library it's also recommended not only to export the overlay containing overrides specific to this library, but also a composed overlay in the `overlay` attribute (look at some of the mentioned repos for examples).

### fastBuild
By default nixpkgs' Haskell build system builds and runs everything there is in a derivation -- tests, benchmarks, haddock, profiling libraries.
To speed up builds a `fastBuild` function was introduced which only builds the main project with optimizations. You should apply this function your Haskell overrides that you'd need to build anyway.
Don't use it for other overrides in order to make use of binary cache.

`fastBuild` is customizable via

1. passing an `overrides` attribute to the `fastBuild` call (see the code above).
2. an override for `eulerBuild` params (this affects all libraries built with the previous settings and might cause a lot of rebuilds).
The full set of attributes for customisation is described in the following example:

```nix
An example with overriding eulerBuild creation args
_eulerBuild = eulerBuild.override {
  fastBuildParams = {
    runTests = true;
    enableProfiling = false;
    buildDocs = false;
    enableBenchmarks = false;
    dontOptimize = true;
  };
};
```
There is also a `fastBuildExternal` which is used for non-euler overrides (like beam, hedis, etc.). Currently it is the same as `fastBuild`, but maybe later we'd want to change defaults for it (require to build and run tests for example).

What if you used `eulerBuild.override`, (for example) enabled tests and they fail for some package?
In this case you should add a call to `self.haskell.lib.dontCheck` to your definition of an override (so a pretty standard procedure for users of nixpkgs haskell infra).

### Using a shell.nix
`eulerBuild` provides a `mkShell` function with the following arguments:

1. `drvPath`: path to your main `default.nix` file
2. `drvName`: name of you derivation (name that you gave to a new haskell package)
3. `haskellPackagesTools`: what extra haskell packages should be available inside the shell.
    Can be used to add `hlint`, `cabal-fmt`, etc.
4. `tools`: what other extra packages should be available in the nix-shell.
    Can be used for `ghcid`, `diffutils`, etc.

An example from `euler-hs`:
```nix
haskellPackagesTools =
  with pkgs.haskellPackages;
  [
    hlint
    cabal-fmt
    nixfmt
    stylish-haskell
  ];
tools = [];

mkShell = eulerBuild.mkShell {
  drvPath = ./default.nix;
  drvName = "euler-hs";
  inherit haskellPackagesTools;
  inherit tools;
};
```

Don't forget to add `inherit mkShell;` to the result of your `default.nix` and put `(import ./default.nix {}).mkShell` in your `shell.nix` file.

## Fetching dependencies with git
To fetch internal repositories use a builtin `fetchGit` function.
You need to load your ssh key to the `ssh-agent` in order for `fetchGit` to work properly.

There is also an older `fetchgit` builtin function (yes, it's one uppercase letter difference) which has a bit different interface and can be used to fetch public git deps.
(TODO: add a note about ssh/config and `fetchgit`)

Another difference is that `fetchgit` fetches dependencies at the derivation's "build time" and `fetchGit` at the expression evaluation time.

By default `fetchGit` fetches only the default ref (which in our case is most likely `master`).
If you want to use some commit from non-default branch you can specify the `ref` attribute:
```nix
fetchGit {
  url = "...";
  ref = "mybranch";
  rev = "commit hash that you need";
};
```
for freezed tags you can omit revision, as fetchGit will use the latest revision if rev attribute is omitted:
```nix
fetchGit {
  url = "...";
  ref = "mytag-1.2.3";
};
```

To fetch from a public GitHub repo you can use a nixpkgs function `fetchFromGitHub` which fetches an archive with the required commit (if you don't use submodules).

# Build with stack
Does not differ much from a standard workflow, don't forget to add appropriate `extra-deps`, as we are currently using the forked versions of the following libraries:

- `beam`
- `beam-mysql`
- `hedis` 

Unfortunately you can use tags with releases in a `stack.yaml` file, because in order to fetch an archive with the required tag https://bitbucket.org/juspay/euler-hs/get/my-tag-name.tar.gz you need to provide your username and password; ssh keys don't work here.
Stack also doesn't support fetching a specific git ref, so you have to use commit hashes.

You can look at `stack.yaml` files of libraries that you want to use and copy `extra-deps` used.
These repos use a lot of euler libraries and should usually have up to date stack files you can use an examples:

  - https://bitbucket.org/juspay/euler-api-order/src/master/stack.yaml
  - https://bitbucket.org/juspay/euler-api-gateway/src/master/stack.yaml

### Nix integration
Some of the libraries used by `euler-hs` require presence of mysql, postgres, openssl and zlib packages in order to build. You can either install it globally via your package manage, or rely on nix and use stack-nix integration the following to your `stack.yaml`:
```yaml
nix:
  enable: false
  packages: [mysql57, openssl, zlib, postgresql]
```

After that you can do `stack --nix build` and get a working build (`stack --nix test` for test, etc.).
