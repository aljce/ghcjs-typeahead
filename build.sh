#!/usr/bin/env bash
set -e
echo Building Haskell...
case "$1" in
    -c|--configure) configure="cabal configure --ghcjs\n";;
esac
echo -e "$configure cabal build\nexit" |
    ./deps/reflex-platform/work-on ghcjs ./.
INDEX="$(find dist -name index.html | grep jsexe | head -n1)"
GHCJSEXE="$(dirname $INDEX)"
echo Build Webassets...
cd www
if [ ! -e bower_components ]; then
    bower install
fi
[ -e dist ] && rm -r dist
mkdir dist
cp -r ../$GHCJSEXE/* dist
echo 'Build Successful'
