self: super: {
  abcde = super.abcde.overrideAttrs (old: rec {
    configurePhase = old.configurePhase + ''
      substituteInPlace "abcde" \
        --replace-fail "$out/etc/abcde.conf" "${self.etcdots}/etc/abcde.conf"
    '';
  });

  alacritty-wrapped = super.callPackage ./alacritty { };

  channel-search = super.callPackage ./channel-search { };

  diss = super.callPackage ./diss { };

  dmenu_diss = super.callPackage ./dmenu_diss { };

  dmenu_cdpath = super.callPackage ./dmenu_cdpath { };

  dmenu_fontpreview = super.callPackage ./dmenu_fontpreview { };

  dmenu_pass_otp = super.callPackage ./dmenu_pass_otp { };

  dotinit = super.callPackage ./dotinit { };

  dunst = super.callPackage ./dunst {
    dunst = super.dunst;
    iconThemes = [
      self.hicolor-icon-theme
      self.adwaita-icon-theme
    ];
  };

  computables = super.callPackage ./computables {
    alacritty = self.alacritty-wrapped;
  };

  emacseverywhere = super.callPackage ./emacseverywhere { };

  emc = super.callPackage ./emc { };

  etcdots = super.callPackage ./etcdots { };

  flaccurate = super.callPackage ./flaccurate { };

  interactive-editor = super.callPackage ./interactive-editor { };

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

  nsxiv = super.callPackage ./nsxiv { nsxiv = super.nsxiv; };

  openvpn_dmenu = super.callPackage ./openvpn_dmenu { };

  pactl_source_mute = super.callPackage ./pactl_source_mute { };

  pactl_sink_dmenu = super.callPackage ./pactl_sink_dmenu { };

  pick-one-color = super.callPackage ./pick-one-color { };

  udisks_dmenu = super.callPackage ./udisks_dmenu { };

  resound = super.callPackage ./resound { };

  rover = super.callPackage ./rover { };

  screencast = super.callPackage ./screencast { };
  screenshot = super.callPackage ./screenshot { };

  st = super.st.overrideAttrs (old: {
    src = self.fetchurl {
      url = "https://github.com/ivanbrennan/st/archive/e47c9d1ccdce70b54d8f792d047edf03efac65d7.tar.gz";
      sha256 = "1b5wzh7qpxnn0py53q13kc4sn2whxffh3j3dcgw89yp1jfbnqvsn";
    };
  });

  start-hyprland-session = super.callPackage ./start-hyprland-session { };

  start-xsession = super.callPackage ./start-xsession { };

  trayer-padding-icon = super.callPackage ./trayer-padding-icon { };

  vln = super.callPackage ./vln { };
}
