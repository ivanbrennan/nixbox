{ stdenv
, fetchFromGitHub
, stow
}:

stdenv.mkDerivation {
  name = "vln";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "vln";
    rev = "f1652f46da59dbf9e256083927ce1c0f72a31e9d";
    sha256 = "1f97iv7zad8ba847jks0wnxs9qgy4kpn8ln5jhgm094jpps04vkb";
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
