pkgs: {
  dotvim = pkgs.callPackage ./dotvim { };
  linuxPackages = (pkgs.linuxPackagesFor pkgs.linuxPackages.kernel).extend (
    self: super:
    {
      i8042_debounce = super.callPackage ./i8042-debounce { };
    }
  );
  sysless = pkgs.callPackage ./sysless { };
  vln = pkgs.callPackage ./vln { };
}
