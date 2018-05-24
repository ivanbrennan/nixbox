{ stdenv
, fetchFromGitHub
}:

stdenv.mkDerivation {
  name = "nice-backgrounds";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "dotfiles";
    rev = "0dca1ea67896ad48d7d98c9f4b8d773489e7bba6";
    sha256 = "1s6l1qpz9cb8ry9yk6y3ci6ydx6gwmcgzmq6k9jvjgp1cqyvamrb";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase = ''
    install -D -m644 $src/backgrounds/Godafoss_Iceland.jpg \
      $out/share/backgrounds/gnome/Godafoss_Iceland.jpg
  '';

  meta = {
    description = "Nice background images";
  };
}
