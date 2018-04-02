{ stdenv
, fetchFromGitHub
, less
}:

stdenv.mkDerivation {
  name = "etcdots";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "dotfiles";
    rev = "d254dcbe275c761a3f2b64719c9e6dd75e959930";
    sha256 = "1s5qgc6kv0r73shk05as17mnkk5zlqm313f9awmhmgdvx70i3a2l";
  };

  buildInputs = [ less ];

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

    lesskey --output=$out/etc/sysless -- $src/lesskey
  '';

  meta = {
    description = "etc config files";
  };
}
