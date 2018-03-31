[ (self: super: {
    dotvim = super.callPackage ./dotvim { };

    etcdots = super.callPackage ./etcdots { };

    linuxPackages = (super.linuxPackagesFor super.linuxPackages.kernel).extend (
      self': super': {
        i8042_debounce = super'.callPackage ./i8042-debounce { };
      }
    );

    sysless = super.callPackage ./sysless { };

    vln = super.callPackage ./vln { };
  })
]
