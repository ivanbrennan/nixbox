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
              rev    = "5c1185a4cf8000e5bf55c9b42fccac1956239109";
              sha256 = "1pp8gbrwhrl1csv94qxlr674qg38fg93awlp5nb0nhh7hg4zmaii";
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
