pkgs: {
  sysless = pkgs.callPackage ./sysless { };
  linuxPackages = (pkgs.linuxPackagesFor pkgs.linuxPackages.kernel).extend (
    self: super:
      { i8042_debounce = super.callPackage ./i8042-debounce { };
    }
  );
}
