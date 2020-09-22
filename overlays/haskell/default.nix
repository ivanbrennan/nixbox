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
              rev    = "9062bb7e8f0bf83e288aef6896fa73d47e21bc53";
              sha256 = "1s0j1h1xnfjp4hms9rwc0y2j56s9n257m0kd8daw624xrdphxyw9";
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
