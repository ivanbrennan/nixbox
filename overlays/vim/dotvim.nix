{ stdenv
, fetchFromGitHub
}:

stdenv.mkDerivation {
  name = "dotvim";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "dotvim";
    rev = "97bed1f2e534ea583c51b4317d8d9ea53c4728cf";
    sha256 = "0bxhqvzswg37dswnqqgrwq4fwc1jb3lks85fb26i77has57lzv2y";
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
