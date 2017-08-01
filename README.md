
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


## Running the service locally

See [docs/running.md](running).

## I18n

Instead of translateable texts, use translation keys.  Translation
keys are fresh identifiers of type `TKey`.  If you need a new
translation key:

1. think of a name (underscores are ok).
2. use this name in an expression of type `(__ some_t_key)` *without declaring it*.
3. make sure to import `Refine.Frontend.TKey`.
4. run `./build build-frontend-trans`.

Search the frontend code for `__` for examples.


## acceptance tests.

There are two programs in ./accept: accept.hs and selenium.hs.  The
simplest way to run them should be this:

```
export SELENIUM_HUB_PORT=4444
export SELENIUM_NODE_PORT=5555
export REFINE_APP_PORT=8086  # must be available
export REFINE_RUN_APP=True  # toggles whether to run selenium from inside accept (True), or assume it's already running (False)
./build accept
```

If you want to watch the browser while it is happening, set `DISPLAY`
to your X server (or do nothing if X is already available in your
shell).  If you set `DISPLAY` to a server that is not available (say,
':7'), a headless X server will be started there.

Building the server takes almost literally forever.  If you want to
write tests and re-run them with a shorter feedback loop without
re-building the backend, you can try this (after having run the
above):

```haskell
cd ./accept
stack exec -- selenium start
stack exec -- selenium status
export REFINE_RUN_APP=False
stack exec -- accept
...  # (edit accept.hs)
stack build --fast
stack exec -- accept --match='###'  # only run test cases whose description contains ###
...  # (repeat)
```

At the time of writing this, the test cases are all included in
accept.hs.  This may change in the future, and tests may fan out into
sub-modules.

`accept` and `selenium` need to be compiled (as opposed to run as a
stack-script inside runghc) because of some subtleties in the way
processIds and kill signals are propagated into and out of child
processes.  At least i couldn't get it to work as a script.
