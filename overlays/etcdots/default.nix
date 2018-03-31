{ stdenv
, fetchFromGitHub
, less
}:

stdenv.mkDerivation {
  name = "etcdots";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "dotfiles";
    rev = "251838e8ff7eb7e54ce019701464a240fee3cc09";
    sha256 = "0q8jmk42i6z073zmdgjfr46a86gss0n3phpywxzwarww8vcmfin9";
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
