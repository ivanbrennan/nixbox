{ stdenv
, fetchFromGitHub
}:

stdenv.mkDerivation {
  name = "etcdots";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "dotfiles";
    rev = "8ead7cf23467f67dcb3c07c8bf2231b0655d82ad";
    sha256 = "0yaj3jbanlh0jirq2h1ijp20zb359nhzyxh9l85vnr2p67cnzj09";
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

    cp -r $src/icons $out/share/icons
  '';

  meta = {
    description = "etc config files";
  };
}
