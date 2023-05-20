let
  pinned = attrs: (import (builtins.fetchTarball attrs) { overlays = []; });

in self: super: {
  abcde = super.abcde.overrideAttrs (old: rec {
    configurePhase = old.configurePhase + ''
      substituteInPlace "abcde" \
        --replace "$out/etc/abcde.conf" "${self.etcdots}/etc/abcde.conf"
    '';
  });

  alacritty-wrapped = super.callPackage ./alacritty { };

  bleep = super.callPackage ./bleep { };
  bloop = super.callPackage ./bloop { };

  diss = super.callPackage ./diss { };

  dmenu_diss = super.callPackage ./dmenu_diss { };

  dmenu_cdpath = super.callPackage ./dmenu_cdpath { };

  dmenu_fontpreview = super.callPackage ./dmenu_fontpreview { };

  dotinit = super.callPackage ./dotinit { };

  dunst = super.callPackage ./dunst {
    dunst = super.dunst;
    iconThemes = [
      self.hicolor-icon-theme
      self.gnome3.adwaita-icon-theme
    ];
  };

  computables = super.callPackage ./computables {
    alacritty = self.alacritty-wrapped;
  };

  emacseverywhere = super.callPackage ./emacseverywhere { };

  emc = super.callPackage ./emc { };

  etcdots = super.callPackage ./etcdots { };

  flaccurate = super.callPackage ./flaccurate { };

  fly =
    (pinned {
      url = "https://github.com/NixOS/nixpkgs/archive/331c54c75a5cc8a795893c1f31524520a9dadb4d.tar.gz";
      sha256 = "0ld59zlci23q89y4lq6f1hd7vwqg80swjn0rg1248w0jfahq046w";
    }).fly;

  gpick =
    (pinned {
      url = "https://github.com/NixOS/nixpkgs/archive/d2042f91c1ad953825556be3ec2e53cf9a91fc77.tar.gz";
      sha256 = "0nla2y1j4lfiqixj87y666c5v1n1zza45f3y0d2kr8sidb1ag62i";
    }).gpick;

  interactive-editor = super.callPackage ./interactive-editor { };

  kubernetes-helm =
    (pinned {
      url = "https://github.com/NixOS/nixpkgs/archive/a071bfa7e7bbd62e1b43830e5e79d8b36afe5fa6.tar.gz";
      sha256 = "0yl2bsan5x69a7z6r1fb8zlv4zpibq67pb2x45jjp7wx42ssdkq2";
    }).kubernetes-helm;

  kubectl =
    (pinned {
      url = "https://github.com/NixOS/nixpkgs/archive/23d785aa6f853e6cf3430119811c334025bbef55.tar.gz";
      sha256 = "00fvaap8ibhy63jjsvk61sbkspb8zj7chvg13vncn7scr4jlzd60";
    }).kubectl;

  _1password = super._1password.overrideAttrs (old: rec {
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
      src = self.fetchFromGitLab {
        owner = "ivanbrennan";
        repo = "caps2esc";
        rev = "3f9a39c6ce31a626682f05a3880f18e2d08fe3f3";
        sha256 = "sha256-j+fj6jOjv1oc84mMY7fXITJeaEMPEnq0CFyUPywxswc=";
      };
    });
  };

  openvpn_dmenu = super.callPackage ./openvpn_dmenu { };

  pick-one-color = super.callPackage ./pick-one-color { };

  udisks_dmenu = super.callPackage ./udisks_dmenu { };

  resound = super.callPackage ./resound { };

  rover = super.callPackage ./rover { };

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

  screencast = super.callPackage ./screencast { };
  screenshot = super.callPackage ./screenshot { };

  st = super.st.overrideAttrs (old: {
    src = self.fetchurl {
      url = "https://github.com/ivanbrennan/st/archive/e47c9d1ccdce70b54d8f792d047edf03efac65d7.tar.gz";
      sha256 = "1b5wzh7qpxnn0py53q13kc4sn2whxffh3j3dcgw89yp1jfbnqvsn";
    };
  });

  trayer-padding-icon = super.callPackage ./trayer-padding-icon { };

  vln = super.callPackage ./vln { };
}
