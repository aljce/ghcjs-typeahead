#!/usr/bin/env bash
set -e
cd www
if [ ! -e bower_components ]; then
    bower install
fi
stack build
[ -e dist ] && rm -r dist
mkdir dist
INDEX="$(find ../.stack-work -name index.html | grep bin | head -n1)"
OUT="$(dirname $INDEX)"
cp -r $OUT/* dist
echo 'Build Successful'
