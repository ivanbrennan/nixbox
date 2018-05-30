self: super: {
  sumall-cli = super.callPackage ./sumall-cli { };

  sumall-env = super.buildEnv {
    name = "sumall-env";
    paths = with self; [
      sumall-cli
    ];
  };
}
