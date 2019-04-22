{ stdenv
, fetchFromGitHub
}:

stdenv.mkDerivation {
  name = "etcdots";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "dotfiles";
    rev = "99b048fd257c29835940541d4dc31035a137d6a1";
    sha256 = "11x9vlc82chikz16vs4c2iaqzj7pijm0f2s965szq947plfvwrka";
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
