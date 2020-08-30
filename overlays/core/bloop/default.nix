{ writers, ghc, haskellPackages }:

writers.writeHaskell "bloop" {
  ghc = ghc;
  libraries = [ haskellPackages.QuickCheck ];
} ./bloop.hs
