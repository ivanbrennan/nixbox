self: super: {
  etcdots = super.callPackage ./etcdots { };

  interactive-editor = super.callPackage ./interactive-editor { };

  # get certificate fix
  # https://github.com/lastpass/lastpass-cli/pull/410
  lastpass-cli = super.lastpass-cli.overrideAttrs (oldAttrs: rec {
    version = "1.3.1";
    src = super.fetchFromGitHub {
      owner = "lastpass";
      repo = "lastpass-cli";
      rev = "v${version}";
      sha256 = "11drzmfdvb8ydw1dxaz9zz8rk0jjqmfv076vydz05qqvgx59s38h";
    };
  });

  linuxPackages = (super.linuxPackagesFor super.linuxPackages.kernel).extend (
    self': super': {
      i8042_debounce = super'.callPackage ./i8042-debounce { };
    }
  );

  nice-backgrounds = super.callPackage ./nice-backgrounds { };

  vln = super.callPackage ./vln { };
}
