#!/bin/bash

set -e

export STEM=$1
if [ "$STEM" == "" ]; then
    echo "give one filename"
fi

echo -n "creating ${STEM}_*.svg..."

cp {00_joker,${STEM}}_bright.svg
cp {00_joker,${STEM}}_dark.svg
cp {00_joker,${STEM}}_RO.svg

echo " [ok]"
