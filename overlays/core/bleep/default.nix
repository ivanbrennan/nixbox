{ haskell, haskellPackages }:

let
  pkg = import (
    builtins.fetchTarball https://github.com/ivanbrennan/bleep/archive/0.1.4.0.tar.gz
  ) { };
in
  haskell.lib.buildStrictly (haskell.lib.justStaticExecutables pkg)
