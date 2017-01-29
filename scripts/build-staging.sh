#!/bin/bash

set -e
cd `dirname $0`/..

# build
./test-all.sh --clean

# build frontend for production
cd pkg/frontend
npm install
stack build
make
npm run build

# start server
cd ../backend
stack exec -- refine server.conf
