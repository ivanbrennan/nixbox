{ stdenv
, fetchFromGitHub
, stow
}:

stdenv.mkDerivation {
  name = "vln";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "vln";
    rev = "b86aa6b6f2cfcd6ed4613b4f18bb60c734d6e49e";
    sha256 = "1vvw2d0wfg6ra96740kqbc10fjbnmp9x6v0j1yqiqm3vz8yz4qjw";
  };

  propagatedBuildInputs = [ stow ];

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase = ''
    install -D -m755 $src/vln $out/bin/vln
    install -D -m644 $src/completions/vln $out/share/bash-completion/completions/vln
  '';

  meta = {
    description = "Manage ~/.vim symlinks";
  };
}
