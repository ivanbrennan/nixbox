self: super:
{
  haskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: hsuper // {
      xmonad-contrib =
        super.haskell.lib.dontHaddock
        # Overriding the entire package, rather than just its "src" attribute,
        # is necessary due to some dependency changes (such as the introduction
        # of hspec). Once those changes have made it into nixpkgs, we should be
        # able to return to the simpler overrideAttrs (commented out below).
        ( hsuper.callPackage
            ({ mkDerivation, base, bytestring, containers, directory, filepath
             , hspec, mtl, old-locale, old-time, process, QuickCheck, random
             , stdenv, unix, utf8-string, X11, X11-xft, xmonad
             }:
             mkDerivation {
               pname = "xmonad-contrib";
               version = "0.16";
               src = super.fetchFromGitHub {
                 owner  = "xmonad";
                 repo   = "xmonad-contrib";
                 rev    = "747202a214a342162707188738c7fad32b47928b";
                 sha256 = "1b40gx404k5kj5qaj00mrdzxj4lr7ra22p30cv639mwsf0a92gv6";
               };
               libraryHaskellDepends = [
                 base bytestring containers directory filepath mtl old-locale
                 old-time process random unix utf8-string X11 X11-xft xmonad
               ];
               testHaskellDepends = [
                 base containers directory hspec mtl process QuickCheck unix
                 utf8-string X11 xmonad
               ];
               homepage = "http://xmonad.org/";
               description = "Third party extensions for xmonad";
               license = super.lib.licenses.bsd3;
             }) {}
        );
        # ( hsuper.xmonad-contrib.overrideAttrs (old: {
        #     src = super.fetchFromGitHub {
        #       owner  = "ivanbrennan";
        #       repo   = "xmonad-contrib";
        #       rev    = "d4e15cddd1c2f42126ce1017efe0cd05d1306d0f";
        #       sha256 = "145ikcm41gp8wka7snb4jlma2jwv9v9lkjnpd9d5qfpird53rg3s";
        #     };
        #   })
        # );
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
