[ (self: super: {
    dotvim = super.callPackage ./dotvim {
      stdenv = self.stdenv;
      fetchFromGitHub = self.fetchFromGitHub;
    };
  })
  (self: super: {
    linuxPackages = (super.linuxPackagesFor super.linuxPackages.kernel).extend (
      inner_self: inner_super: {
        i8042_debounce = super.callPackage ./i8042-debounce {
          stdenv = self.stdenv;
          fetchFromGitHub = self.fetchFromGitHub;
          kernel = inner_self.kernel;
        };
      }
    );
  })
  (self: super: {
    sysless = super.callPackage ./sysless {
      stdenv = self.stdenv;
      fetchFromGitHub = self.fetchFromGitHub;
      less = self.less;
    };
  })
  (self: super: {
    vln = super.callPackage ./vln {
      stdenv = self.stdenv;
      fetchFromGitHub = self.fetchFromGitHub;
      stow = self.stow;
    };
  })
]
