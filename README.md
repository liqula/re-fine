
# staging server

```
https://demo8.aula.de/
login: re-fine
passwd: tAA957Mn4w07ok
```

(for devs: ssh to refine-stage1@zb1 for maintenance)


# Installation and testing

Tested with nodejs-v6.9.15.  Known not to work with nodejs-v7.*!


## Node.js on Debian

To install nodejs-v6.x on Debian execute these commands (as root):

    # Pre-requisites
    apt install apt-transport-https curl
    # Add GPG key for signing packages
    curl -s https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add -
    # Create additionnal sources.list
    echo "deb https://deb.nodesource.com/node_6.x jessie main" > /etc/apt/sources.list.d/nodesource.list
    echo "deb-src https://deb.nodesource.com/node_6.x jessie main" >> /etc/apt/sources.list.d/nodesource.list
    # Update packages cache and install
    apt update && apt install nodejs


## Workaround Issue #40

The following command may be needed to workaround Issue #40 in a fresh build:

    (cd pkg/frontend; npm install)


## Run the build script

Run `./build` to build and test all packages.  (This is also what the
ci does.)

`./build` is also the recommended way to call the shake script
`./Build.hs`.  Any arguments that you pass to the former are passed to
the latter and interpreted as rules.  For example:

```shell
./build hlint            # hlint the entire repo
./build hlint-frontend   # hlint the frontend package
./build test-common      # build the common package and run the test suite
./build run-dev          # run the test server (consult the output for shutdown instructions)
```

The complete set of rules is in `./Build.hs` and should be
self-explanatory (at least for developers :).


## I18n

Instead of translateable texts, use translation keys.  Translation
keys are fresh identifiers of type `TKey`.  If you need a new
translation key:

1. think of a name (underscores are ok).
2. use this name in an expression of type `(__ some_t_key)` *without declaring it*.
3. make sure to import `Refine.Frontend.TKey`.
4. run `./build build-frontend-trans`.

Search the frontend code for `__` for examples.
