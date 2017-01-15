#!/bin/sh

stack install --resolver lts-7.15 shake
./build.sh setup
./build.sh
