#!/bin/bash

runhaskell Setup.hs configure --prefix=$HOME/.cabal --user	&&
runhaskell Setup.hs build									&&
# runhaskell Setup.hs haddock									&&
ghc-pkg unregister texprep
rm -rf ~/.cabal/bin/texprep									&&
rm -rf ~/.cabal/lib/texprep-0.1								&&
runhaskell Setup.hs install
