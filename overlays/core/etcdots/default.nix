{ stdenv
, fetchFromGitHub
}:

stdenv.mkDerivation {
  name = "etcdots";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "dotfiles";
    rev = "575319c32f7d83eb6a5faaaf7969936531e11549";
    sha256 = "0szccychl8rbaz9yk628vc8yzcr4yvj69231y8qhc57nzvgj0j2r";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase = ''
    install -D -m644 $src/lesskey               $out/lesskey

    install -D -m644 $src/git/attributes        $out/etc/gitattributes
    install -D -m644 $src/git/ignore            $out/etc/gitignore
    install -D -m644 $src/shell/inputrc         $out/etc/inputrc
    install -D -m644 $src/tmux/tmux.conf        $out/etc/tmux.conf
    install -D -m644 $src/irbrc                 $out/etc/irbrc
    install -D -m644 $src/abcde/abcde.conf      $out/etc/abcde.conf

    install -D -m644 $src/fzf/key-bindings.bash $out/share/etcdots/key-bindings.bash
  '';

  meta = {
    description = "etc config files";
  };
}
