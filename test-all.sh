#!/bin/sh

# this script builds and tests all packages in this repository.  it is
# called in .gitlab-ci.yaml, but is also intended for developers and
# advanced users.
#
# NOTE: currently, the 'setup' rule installs specific versions of
# hlint and hspec-discover under ~/.local/bin/, which is added to the
# $PATH here.  if you don't want this, you should edit Build.hs before
# you run it.

set -e
export PATH=$HOME/.local/bin:$PATH

stack install --resolver lts-7.15 shake
./build.sh setup
./build.sh
