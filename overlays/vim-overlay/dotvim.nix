{ stdenv
, fetchFromGitHub
}:

stdenv.mkDerivation {
  name = "dotvim";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "dotvim";
    rev = "f3f8686d24e4a8430cc0d80cc6026b5028ef1ec9";
    sha256 = "0i767ij67k2h53wyq06sdbngxiqwlh0rj0igc6bj8s7wv5r3zwq3";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase = ''
    mkdir -p $out
    cp $src/vimrc $out/vimrc
  '';

  meta = {
    description = "A minimal vimrc.";
  };
}
