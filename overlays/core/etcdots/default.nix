{ stdenv
, fetchFromGitHub
}:

stdenv.mkDerivation {
  name = "etcdots";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "dotfiles";
    rev = "54f32cb7b5b69f0d87994f46b0d25dd01e66ab21";
    sha256 = "0vq3w29vj5j64gx6phknq4nvp3vifpyl5jblk8ga4pqvr27fb07l";
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
