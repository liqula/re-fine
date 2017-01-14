#!/bin/sh
mkdir -p .shake
stack exec -- ghc --make Build.hs -rtsopts -with-rtsopts=-I0 -outputdir=.shake -o .shake/build && .shake/build "$@"
