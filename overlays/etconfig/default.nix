{ stdenv
, fetchFromGitHub
}:

stdenv.mkDerivation {
  name = "etconfig";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "dotfiles";
    rev = "dcd560a51be59dcf1f9b10d986f76b343cd932e6";
    sha256 = "0vxq804y6idmm2785y1j2zan0317bbvpzpsb7cii3lyp30wx224y";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase = ''
    mkdir -p $out/etc
    cp $src/git/attributes $out/etc/gitattributes
    cp $src/git/ignore     $out/etc/gitignore
    cp $src/shell/inputrc  $out/etc/inputrc
  '';

  meta = {
    description = "etc config files";
  };
}
