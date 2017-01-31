#!/bin/bash

set -e
cd `dirname $0`/..

export ROOT=`pwd`

./build setup

# build backend.
./build build-backend

# build frontend for production
./build build-frontend
cd $ROOT/pkg/frontend
npm run build

# start server
cd $ROOT/pkg/backend
stack exec -- refine server.conf
