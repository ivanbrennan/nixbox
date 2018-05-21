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
    install -D $src/lesskey               $out/lesskey

    install -D $src/git/attributes        $out/etc/gitattributes
    install -D $src/git/ignore            $out/etc/gitignore
    install -D $src/shell/inputrc         $out/etc/inputrc
    install -D $src/tmux/tmux.conf        $out/etc/tmux.conf
    install -D $src/irbrc                 $out/etc/irbrc

    install -D $src/fzf/key-bindings.bash $out/share/etcdots/key-bindings.bash
  '';

  meta = {
    description = "etc config files";
  };
}
