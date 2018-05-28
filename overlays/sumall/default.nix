self: super: {
  sumall-cli = super.callPackage ./sumall-cli { pkgs = self; };

  sumall-completion = super.callPackage ./sumall-completion { pkgs = self; };
}
