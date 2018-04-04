{ stdenv
, fetchFromGitHub
, less
}:

stdenv.mkDerivation {
  name = "etcdots";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "dotfiles";
    rev = "ac11b4af403b7c30d7c1a8ab7327d8f8c88e9914";
    sha256 = "1yd3z17cp1l041bwyh14l27l55wrf73y09zaw0ip5nn3kyc8i2f9";
  };

  buildInputs = [ less ];

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase = ''
    mkdir -p $out/etc

    cp $src/git/attributes $out/etc/gitattributes
    cp $src/git/ignore     $out/etc/gitignore
    cp $src/shell/inputrc  $out/etc/inputrc
    cp $src/tmux/tmux.conf $out/etc/tmux.conf

    lesskey --output=$out/etc/sysless -- $src/lesskey
  '';

  meta = {
    description = "etc config files";
  };
}
