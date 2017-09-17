#!/bin/sh
mkdir -p _shake
# Note: ghc will only rebuild the binary if ShakeBuild.hs has changed (or it
# hasn't been built previously).
ghc --make ShakeBuild.hs -rtsopts -with-rtsopts=-I0 -outputdir=_shake -o _shake/build && _shake/build "$@"

