#!/bin/bash

set -e
cd `dirname $0`

export STEM=$1
if [ "$STEM" == "" ]; then
    echo "give one filename"
fi

echo -n "creating ${STEM}_*.svg..."

cp {00_joker,${STEM}}_bright.svg
cp {00_joker,${STEM}}_dark.svg
cp {00_joker,${STEM}}_RO.svg

git add ${STEM}_{bright,dark,RO}.svg

echo " [ok]"

echo -n "updating scss classes..."
../scss/2-generic/generate_icons_scss
echo " [ok]"
