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
