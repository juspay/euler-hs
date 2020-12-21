# Building EulerHS Framework

## Prerequisites

EulerHS framework uses several external libraries you need to install first:

- binutils
- libssl-dev
- libpq-dev
- libmysqlclient-dev
- libsqlite-dev
- postgresql
- postgresql-server-dev-all
- mysql-server
- ... maybe some others

### Linux

Ubuntu libraries can be installed from the general repo:

`sudo apt install binutils`
`sudo apt install libssl-dev`
`sudo apt install libpq-dev`
`sudo apt install libmysqlclient-dev`
`sudo apt install libsqlite-dev`
`sudo apt install postgresql`
`sudo apt install postgresql-server-dev-all`
`sudo apt install mysql-server`

### MacOS

Packages for MacOS can be installed using brew.

N.B., you might meet a problem with MacOS building failure:
`>   Configuring mysql-0.1.7...`
`>   setup: Missing dependencies on foreign libraries:`
`>   * Missing (or bad) C libraries: ssl, crypto`

[Possible solution](https://github.com/depressed-pho/HsOpenSSL/issues/41)

## Building the framework

### Stack

Building with stack is straightforward. Run in the euler-hs dir:

- `stack build`            - build the framework
- `stack test`             - build and test the framework
- `stack build --fast -j4` - build the framework without optimisations

### cabal

> TODO

### nix

> TODO
