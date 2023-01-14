{ stdenv
, fetchFromGitHub
}:

stdenv.mkDerivation {
  name = "etcdots";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "dotfiles";
    rev = "82f4ef216fdd54735afd393ca8f73530415df809";
    sha256 = "sha256-0iFpe78qrs8S5ruViJfdQLnRLlnAENdqck+0IMHQyYw=";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase = ''
    install -D -m644 $src/lesskey               $out/lesskey

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
