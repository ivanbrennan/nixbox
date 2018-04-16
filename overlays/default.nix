[ (self: super: {
    coderay = super.callPackage ./coderay { };

    etcdots = super.callPackage ./etcdots { };

    linuxPackages = (super.linuxPackagesFor super.linuxPackages.kernel).extend (
      self': super': {
        i8042_debounce = super'.callPackage ./i8042-debounce { };
      }
    );

    nice-backgrounds = super.callPackage ./nice-backgrounds { };

    rouge = super.callPackage ./rouge { };

    vln = super.callPackage ./vln { };
  })

  (import ./vim-overlay)
]
