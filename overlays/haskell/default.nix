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
              rev    = "d4e15cddd1c2f42126ce1017efe0cd05d1306d0f";
              sha256 = "145ikcm41gp8wka7snb4jlma2jwv9v9lkjnpd9d5qfpird53rg3s";
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
