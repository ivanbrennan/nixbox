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
              rev    = "9c4c417936a4506566947c91ddd3b1f6af31e412";
              sha256 = "1b4zqf08qwjx4rp3z3919f6aqdfxvk0pmn752xq3pg8l29i2ka5k";
            };
          })
        );
      xmobar =
        super.haskell.lib.dontHaddock
        ( hsuper.xmobar.overrideAttrs (old: {
            src = super.fetchFromGitHub {
              owner  = "jaor";
              repo   = "xmobar";
              rev    = "3d90497d1e680d844e0b640cf72b6ee7b587243a";
              sha256 = "0wna3n0gfwszvpfa6znl41vcnwxaam6mxwwqc59msxzczf1m1wp2";
            };
          })
        );
    };
  };
}
