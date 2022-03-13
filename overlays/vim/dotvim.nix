{ stdenv
, fetchFromGitHub
}:

stdenv.mkDerivation {
  name = "dotvim";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "dotvim";
    rev = "a4a6a9b66df9ba92c0640492d3f250a175b6abec";
    sha256 = "sha256-l6r18ljmMrYyo6WRzNcnqeWO18WDS2hGiqQQ9VOsZp0=";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase = ''
    install -D -m644 $src/vimrc $out/vimrc
  '';

  meta = {
    description = "A minimal vimrc.";
  };
}
