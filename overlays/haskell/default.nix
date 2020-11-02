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
              owner  = "ivanbrennan";
              repo   = "xmobar";
              rev    = "cc414307db83c3cac94e3452209ad32d37295fd7";
              sha256 = "0qw9f0gv14ghac40swis5l3485ki4yf03fda3jk45wppig0xqzi4";
            };
            configureFlags = [
              "-fwith_alsa" "-fwith_conduit" "-fwith_datezone" "-fwith_dbus"
              "-fwith_inotify" "-fwith_iwlib" "-fwith_mpris"
              "-fwith_rtsopts" "-fwith_threaded" "-fwith_utf8" "-fwith_uvmeter"
              "-fwith_weather" "-fwith_xft" "-fwith_xpm"
            ];
          })
        );
    };
  };
}
