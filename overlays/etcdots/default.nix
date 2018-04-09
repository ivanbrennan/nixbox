{ stdenv
, fetchFromGitHub
}:

stdenv.mkDerivation {
  name = "etcdots";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "dotfiles";
    rev = "d73b37fa8c2cf07034fa1b6555c0125b6191d8e3";
    sha256 = "1z86xs95zi2ymlxldyj0mk95dkdx7m08c39qf25xdm411jwsws5h";
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
    cp $src/lesskey        $out/lesskey
  '';

  meta = {
    description = "etc config files";
  };
}
