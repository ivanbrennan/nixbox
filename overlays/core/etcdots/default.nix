{ stdenv
, fetchFromGitHub
}:

stdenv.mkDerivation {
  name = "etcdots";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "dotfiles";
    rev = "bb523ed954a7f2b86e6ab44e8f41d536d38497e2";
    sha256 = "09kbp49hhg70347vkiq7kyqy5j7f9289xmfrw9npm97ymq3895ll";
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
