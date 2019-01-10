#!/bin/bash

## print a log of the currently being built Haskell package

DIR="$HOME/.cabal/logs/ghc-$(ghc --numeric-version)"
cd "$DIR"
pwd
ls -1t | head -n 1 | { read i; echo $i; tail -f $i; }
