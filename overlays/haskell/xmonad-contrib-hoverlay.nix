self: super: hself: hsuper: {
  xmonad-contrib = hsuper.xmonad-contrib.overrideAttrs (old: {
    src = super.fetchFromGitHub {
      owner  = "ivanbrennan";
      repo   = "xmonad-contrib";
      rev    = "58feba91d96fee8339d7cf56fb33537be819eb4b";
      sha256 = "14qzd23xc6iiqqh51yvf6im7r65x8npb21ik7q99qqh2y894xpsi";
    };
  });
}
