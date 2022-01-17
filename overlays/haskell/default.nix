self: super:
{
  haskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: hsuper // {
      # TODO: Check whether this override is still necessary.
      xmobar =
        super.haskell.lib.dontHaddock
        ( hsuper.xmobar.overrideAttrs (old: {
            src = super.fetchFromGitHub {
              owner  = "jaor";
              repo   = "xmobar";
              rev    = "a58e32f7c8af7b03410ab6693019cfc92c9cfca3";
              sha256 = "186h8qlp7gjdh6r8qns0vfsl40qsi2a1r0w6xzzsy4lvslqhc2fx";
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
