{ haskell, haskellPackages }:

let
  pkg = import (
    builtins.fetchTarball https://github.com/ivanbrennan/bleep/archive/0.1.2.0.tar.gz
  ) { };
in
  haskell.lib.buildStrictly (haskell.lib.justStaticExecutables pkg)
