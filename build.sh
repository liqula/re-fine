#!/bin/sh
set -e
cd `dirname $0`
mkdir -p .shake
# (the resolver in the following line has to match the one given in test-all.sh)
stack exec --resolver lts-7.15 -- ghc -j --make Build.hs -threaded -rtsopts -with-rtsopts="-N" -outputdir=.shake -o .shake/build && .shake/build +RTS -N -RTS "$@"
