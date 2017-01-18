#!/bin/sh
set -e
cd `dirname $0`
mkdir -p .shake
stack exec -- ghc -j --make Build.hs -threaded -rtsopts -with-rtsopts="-N" -outputdir=.shake -o .shake/build && .shake/build +RTS -N -RTS "$@"
