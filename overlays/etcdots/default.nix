{ stdenv
, fetchFromGitHub
, less
}:

stdenv.mkDerivation {
  name = "etcdots";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "dotfiles";
    rev = "8445713ac17c44a8d94690c08e2b09ee8e79fb5c";
    sha256 = "0bldj2ddy1plys2y6xgqrjc071d77s43z4l4sr4kjr5pxmjpsql1";
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
