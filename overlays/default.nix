[ (self: super: {
    dotvim = super.callPackage ./dotvim { };

    etcdots = super.callPackage ./etcdots { };

    linuxPackages = (super.linuxPackagesFor super.linuxPackages.kernel).extend (
      self': super': {
        i8042_debounce = super'.callPackage ./i8042-debounce { };
      }
    );

    nice-backgrounds = super.callPackage ./nice-backgrounds { };

    privateVimPlugins = (import ./private-vim-plugins.nix) self;

    vln = super.callPackage ./vln { };
  })
]
