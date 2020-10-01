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
              owner  = "ivanbrennan";
              repo   = "xmobar";
              rev    = "3a67fad558e42db5e4115884f1afe826054b88c9";
              sha256 = "10z03kab14phpd07m2fp3j475bv82pp4gd2gz18wm5nnd6fvn837";
            };
          })
        );
    };
  };
}
