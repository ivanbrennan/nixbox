[ (self: super: {
    dotvim = super.callPackage ./dotvim {
      stdenv = self.stdenv;
      fetchFromGitHub = self.fetchFromGitHub;
    };

    linuxPackages = (super.linuxPackagesFor super.linuxPackages.kernel).extend (
      inner_self: inner_super: {
        i8042_debounce = super.callPackage ./i8042-debounce {
          stdenv = self.stdenv;
          fetchFromGitHub = self.fetchFromGitHub;
          kernel = inner_self.kernel;
        };
      }
    );

    sysless = super.callPackage ./sysless {
      stdenv = self.stdenv;
      fetchFromGitHub = self.fetchFromGitHub;
      less = self.less;
    };

    vln = super.callPackage ./vln {
      stdenv = self.stdenv;
      fetchFromGitHub = self.fetchFromGitHub;
      stow = self.stow;
    };
  })
]
