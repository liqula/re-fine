#!/bin/bash

set -e
cd `dirname $0`/..

./build.sh setup

# build backend.
./build.sh clean
./build.sh build-backend

# build frontend for production
./build.sh build-frontend
npm run build

# start server
cd pkg/backend
stack exec -- refine server.conf

