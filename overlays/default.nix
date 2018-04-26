[ (self: super: {
    etcdots = super.callPackage ./etcdots { };

    interactive-editor = super.callPackage ./interactive-editor { };

    linuxPackages = (super.linuxPackagesFor super.linuxPackages.kernel).extend (
      self': super': {
        i8042_debounce = super'.callPackage ./i8042-debounce { };
      }
    );

    nice-backgrounds = super.callPackage ./nice-backgrounds { };

    vln = super.callPackage ./vln { };
  })

  (import ./vim-overlay)
]
