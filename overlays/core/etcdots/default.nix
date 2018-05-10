{ stdenv
, fetchFromGitHub
}:

stdenv.mkDerivation {
  name = "etcdots";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "dotfiles";
    rev = "3fc41172836006cf74dfb364c1a1e857cd94af34";
    sha256 = "03d1ycrvmshx5sj3h70v6x9dh893q4an290krrc3fa97sipwic9j";
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
