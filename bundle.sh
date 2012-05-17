#!/bin/bash -ex

cabal build
strip dist/build/yesodweb/yesodweb
rm -rf static/tmp
tar czfv yesodweb.keter dist/build/yesodweb/yesodweb config static
