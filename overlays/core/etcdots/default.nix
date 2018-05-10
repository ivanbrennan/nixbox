{ stdenv
, fetchFromGitHub
}:

stdenv.mkDerivation {
  name = "etcdots";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "dotfiles";
    rev = "90123f8db878753394efd0ddf542ce557d7cc5d0";
    sha256 = "1pysp1vpbclx88x8ds26z2m4d59wn640f2db68wlgqszyv6bwdmv";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase = ''
    mkdir -p $out/etc
    mkdir -p $out/share/etcdots

    cp $src/git/attributes $out/etc/gitattributes
    cp $src/git/ignore     $out/etc/gitignore
    cp $src/shell/inputrc  $out/etc/inputrc
    cp $src/tmux/tmux.conf $out/etc/tmux.conf
    cp $src/irbrc          $out/etc/irbrc

    cp $src/lesskey $out/lesskey

    cp $src/fzf/key-bindings.bash $out/share/etcdots/key-bindings.bash
  '';

  meta = {
    description = "etc config files";
  };
}
