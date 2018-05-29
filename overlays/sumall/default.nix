self: super: {
  sumall-cli = super.callPackage ./sumall-cli { pkgs = self; };

  sumall-completion = super.callPackage ./sumall-completion { pkgs = self; };

  sumall-env = super.buildEnv {
    name = "sumall-env";
    paths = with self; [
      sumall-cli
      sumall-completion
    ];
  };
}
