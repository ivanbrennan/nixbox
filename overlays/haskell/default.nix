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
              rev    = "9794f3d225f45a693824804efea165f8824506e4";
              sha256 = "1l308jl1130vmd8s5rp73jqzhqh1mgi5zq90b0r86j95dwyrv2ny";
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
