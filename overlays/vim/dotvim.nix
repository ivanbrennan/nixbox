{ stdenv
, fetchFromGitHub
}:

stdenv.mkDerivation {
  name = "dotvim";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "dotvim";
    rev = "8fb670454032fa7f0f8e8cdc2f8cfe3682854301";
    sha256 = "1ail8j42d8sh4dh37ccjkzmsvylv5aipjvyxn4dys8yg2bf1fi2y";
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
