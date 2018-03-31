{ stdenv
, fetchFromGitHub
}:

stdenv.mkDerivation {
  name = "etcdots";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "dotfiles";
    rev = "8b4df5bc9f0a868078ee0f55eaf9c452fb687466";
    sha256 = "1wz2bkgyq0idvr2jydkbrg7ayjw990gxhv5xvyavcgsylkrdr5b0";
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
    cp $src/tmux/tmux.conf $out/etc/tmux.conf
  '';

  meta = {
    description = "etc config files";
  };
}
