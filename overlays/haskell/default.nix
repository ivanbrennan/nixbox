self: super:
{
  haskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: hsuper // {
      xmonad-contrib =
        super.haskell.lib.dontHaddock
        ( hsuper.xmonad-contrib.overrideAttrs (old: {
            src = super.fetchFromGitHub {
              owner  = "ivanbrennan";
              repo   = "xmonad-contrib";
              rev    = "6f49107d40007efd00bd2eceb47ec9b661168001";
              sha256 = "13rjg1xkd3x6wj532h3y82s2dwx0n08msqx2m3m86r2h27krl8xk";
            };
          })
        );
      xmobar =
        super.haskell.lib.dontHaddock
        ( hsuper.xmobar.overrideAttrs (old: {
            src = super.fetchFromGitHub {
              owner  = "jaor";
              repo   = "xmobar";
              rev    = "e71512b7c961ab379aee98c2f4d65ad4bdd3c5bf";
              sha256 = "165cp3c7nrkhadylfqqbq17n33rc6vq0na4h7wh5vmd9m6qn0qw9";
            };
          })
        );
    };
  };
}
