# How to update to a new version of euler-hs
## 1.10.0.0
This contains significant changes to the nix build structure, so it's important to read the section for nix users.

For stack users there are no big changes, mostly some of the `extra-deps` need to be changed and in order for stack builds to work on NixOS (or if you don't want to mysql and other packages to your system and want to rely on nix) you should add a `nix` attribute to `stack.yaml` as described in the "For stack users" section.

We use forked versions of

  - `beam`
  - `beam-mysql`
  - for more info about `beam-*` changes [see BEAM_NOTES.md](BEAM_NOTES.md).
  - `hedis` for redis clustering support (the work on upstreaming was ongoing but looks like currently is frozen INSERT LINK TO PR)

#### For nix users
It would be helpful to read [BUILD.md](BUILD.md) first to familiarise yourself with the `eulerBuild` and existing overlays.

Use nixpkgs with a GHC version 8.8.3+.
You can copy nixpkgs used currently by all euler-hs libraries from [default.nix](default.nix).

To see how to import libraries, see the working examples from projects using:

  - https://bitbucket.org/juspay/euler-api-gateway/src/master/default.nix
  - https://bitbucket.org/juspay/euler-api-order/src/master/default.nix

Also replace your `shell.nix` with a `(import ./default.nix {}).mkShell` text and see examples above and [BUILD.md](BUILD.md) on how to configure and use it.

We use a tag `EulerHS-1.10.0.0` for all libraries.
So fetch a repo with `ref = "EulerHS-1.10.0.0"`.

For explanation of `ref` and `rev` attributes [see BUILD.md section "Fetching deps with git"](BUILD.md).

If you want to use `eulerBuild` the minimum that you need to fetch is `euler-hs` repository which contains `eulerBuild` implementation (it may be moved to a separate repo, but no definite plans yet).

To see how to add your package, add your overlay, see what overlays different libraries provide please refer to [BUILD.md](BUILD.md).

As usual working examples should be in:

- https://bitbucket.org/juspay/euler-api-gateway/src/master/nix
- https://bitbucket.org/juspay/euler-api-order/src/master/nix

###### Important note about certain overrides
Please see a [BUILD.md](BUILD.md) section "Important note about overrides".
The brief version is that if you use a package that depends on `haskell-src-exts` you may get a cabal mismatched deps error. See the linked doc for into on how to fix that.

#### For stack users
1. We are now using GHC 8.8 and resolver 15.15.
2. If you are using `juspay/beam-mysql2` you can go back to `juspay/beam-mysql` as `beam-mysql2` will be deleted in the near future.
3. Look at `stack.yaml` files of libraries that you want to use and copy `extra-deps` used.
4. These repos use a lot of euler libraries and should usually have up to date stack files:

    - https://bitbucket.org/juspay/euler-api-order/src/master/stack.yaml
    - https://bitbucket.org/juspay/euler-api-gateway/src/master/stack.yaml
    
5. Some of the libraries used by `euler-hs` require presence of mysql, postgres, openssl and zlib packages in order to build. You can either install it globally via your package manage, or rely on nix and use stack-nix integration the following to your `stack.yaml`:
```yaml
nix:
  enable: false
  packages: [mysql57, openssl, zlib, postgresql]
```

After that you can do `stack --nix build` and get a working build (`stack --nix test` for test, etc.).

If you are using euler libraries that are fetched remotely (like in the `euler-api-gateway` example) then in order to update you should take a look at what commit does `EulerHS-1.10.0.0` tag refer to (https://bitbucket.org/juspay/euler-hs/commits/tag/EulerHS-1.10.0.0) and change your stack file with that hash.

Alternatively fetch a git repo and do a `git show EulerHS-1.10.0.0` this way you'll also get the required commit hash.
