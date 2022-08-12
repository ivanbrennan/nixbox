{ pkgs, lib, ... }:

let
  cachix = import ./cachix.nix { inherit pkgs lib; };

  adapt = attrs@{ nix, ... }:
    let
      nix' = builtins.removeAttrs nix [
        "binaryCaches"
        "binaryCachePublicKeys"
      ];
      s0 = nix.settings or {};
      s1 =
        if nix ? binaryCaches
        then { substituters = nix.binaryCaches; }
        else {};
      s2 =
        if nix ? binaryCachePublicKeys
        then { trusted-public-keys = nix.binaryCachePublicKeys; }
        else {};
    in
      attrs // {
        nix = nix' // { settings = s0 // s1 // s2; };
      } // (
        if attrs ? imports
        then { imports = builtins.map (x: adapt (import x)) attrs.imports; }
        else {}
      );
in
  adapt cachix
