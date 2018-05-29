self: super: {
  sumall-cli = super.callPackage ./sumall-cli { };

  sumall-completion = self.python2.pkgs.argcomplete;

  sumall-env = super.buildEnv {
    name = "sumall-env";
    paths = with self; [
      sumall-cli
      sumall-completion
    ];
  };
}
