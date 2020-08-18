self: super: {
  abcde = super.abcde.overrideAttrs (old: rec {
    configurePhase = old.configurePhase + ''
      substituteInPlace "abcde" \
        --replace "$out/etc/abcde.conf" "${self.etcdots}/etc/abcde.conf"
    '';
  });

  alacritty-wrapped = super.callPackage ./alacritty { };

  bleep = super.callPackage ./bleep { };

  etcdots = super.callPackage ./etcdots { };

  flaccurate = super.callPackage ./flaccurate { };

  interactive-editor = super.callPackage ./interactive-editor { };

  kubernetes-helm-2_11 =
    (import (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/a071bfa7e7bbd62e1b43830e5e79d8b36afe5fa6.tar.gz";
      sha256 = "0yl2bsan5x69a7z6r1fb8zlv4zpibq67pb2x45jjp7wx42ssdkq2";
    }) { }).kubernetes-helm;

  stack-1_9_3 =
    (import (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/bc94dcf500286495e3c478a9f9322debc94c4304.tar.gz";
      sha256 = "1siqklf863181fqk19d0x5cd0xzxf1w0zh08lv0l0dmjc8xic64a";
    }) { }).stack;

  _1password-1_4_0 = super._1password.overrideAttrs (old: rec {
    version = "1.4.0";
    src = super.fetchzip {
      url = "https://cache.agilebits.com/dist/1P/op/pkg/v${version}/op_linux_amd64_v${version}.zip";
      sha256 = "1r4wm48ichbq208xzfp86ykspfsizxay06nvsyj2rm789km296fl";
      stripRoot = false;
    };
  });

  linuxPackages = (super.linuxPackagesFor super.linuxPackages.kernel).extend (
    self': super': {
      i8042_debounce = super'.callPackage ./i8042-debounce { };
    }
  );

  nice-backgrounds = super.callPackage ./nice-backgrounds { };

  nice-icons = super.buildEnv {
    name = "nice-icons";
    paths = [ self.etcdots ];
    pathsToLink = ["/share"];
  };

  interception-tools-plugins = super.interception-tools-plugins // {
    caps2esc = super.interception-tools-plugins.caps2esc.overrideAttrs (old: {
      name = "interception-tools-caps2esc-0.1.3";
      src = self.fetchurl {
        url = "https://gitlab.com/mar04/caps2esc/repository/3f9a39c6ce31a626682f05a3880f18e2d08fe3f3/archive.tar.gz";
        sha256 = "0w58z0b16hr52sjynkpqd1v090sgyd5glwc8wb248j5l8q2b50zv";
      };
    });
  };

  rxvt_unicode = super.rxvt_unicode.overrideAttrs (old: {
    version = "2020-02-12";
    src = super.fetchcvs {
      cvsRoot = ":pserver:anonymous@cvs.schmorp.de/schmorpforge";
      module = "rxvt-unicode";
      date = "2020-02-12";
      sha256 = "0n8z3c8fb1pqph09fnl9msswdw2wqm84xm5kaax6nf514gg05dpx";
    };
  });

  urxvt_perls = super.urxvt_perls.overrideAttrs (old: rec {
    name = "urxvt-perls-${version}";
    version = "2.3";
    src = super.fetchFromGitHub {
      owner = "muennich";
      repo = "urxvt-perls";
      rev = version;
      sha256 = "0xvwfw7965ghhd9g6rl6y6fgpd444l46rjqmlgg0rfjypbh6c0p1";
    };
    installPhase = ''
      mkdir -p $out/lib/urxvt/perl
      cp keyboard-select $out/lib/urxvt/perl
    '';
  });

  screenshot = super.callPackage ./screenshot { };

  st = super.st.overrideAttrs (old: {
    src = self.fetchurl {
      url = "https://github.com/ivanbrennan/st/archive/e47c9d1ccdce70b54d8f792d047edf03efac65d7.tar.gz";
      sha256 = "1b5wzh7qpxnn0py53q13kc4sn2whxffh3j3dcgw89yp1jfbnqvsn";
    };
  });

  vln = super.callPackage ./vln { };
}
