[ (self: super: {
    etcdots = super.callPackage ./etcdots { };

    linuxPackages = (super.linuxPackagesFor super.linuxPackages.kernel).extend (
      self': super': {
        i8042_debounce = super'.callPackage ./i8042-debounce { };
      }
    );

    nice-backgrounds = super.callPackage ./nice-backgrounds { };

    vln = super.callPackage ./vln { };
  })

  (import ./vim-overlay)

  (self: super: {
    fzf = super.fzf.overrideAttrs (oldAttrs: rec {
      src = super.fetchFromGitHub {
        owner = "junegunn";
        repo = "fzf";
        rev = "6eac4af7db54b2b736a2ede928b14456c0496711";
        sha256 = "0mskwh231c40cqia1y5cy02m52a3grk5d72bcir5idxrj55ip6vz";
      };
    });
  })
]
