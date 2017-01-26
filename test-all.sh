#!/bin/bash

# this script builds and tests all packages in this repository.  it is
# called in .gitlab-ci.yaml, but is also intended for developers and
# advanced users.
#
# NOTE: currently, the 'setup' rule installs specific versions of
# hlint and hspec-discover under ~/.local/bin/, which is added to the
# $PATH here.  if you don't want this, you should edit Build.hs before
# you run it.

set -e
cd `dirname $0`
export PATH=$HOME/.local/bin:$PATH

function assert_os_package() {
    if [ "`uname`" == "Linux" ]; then
       dpkg -l | grep -q $1 || ( echo "please install $1"; exit 1 )
    fi
}

assert_os_package libcurl4-gnutls-dev
assert_os_package stack

function assert_cmd() {
    which $1 > /dev/null || \
        ( echo "please make sure $1 is in your path." >&2; false )
}

assert_cmd node
assert_cmd npm
# REMARKS
# - node should be 6.x.x or higher.
# - npm comes with node, so no extra version constraint here.
# - on debian, the name for the node executable is nodejs, don't let that confuse you!

stack setup --resolver lts-7.15
stack install --resolver lts-7.15 shake
./build.sh setup
./build.sh dist-clean
./build.sh
