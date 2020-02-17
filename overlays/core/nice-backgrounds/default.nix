{ stdenv
, fetchFromGitHub
}:

stdenv.mkDerivation {
  name = "nice-backgrounds";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "dotfiles";
    rev = "2c5c3429837539f5a6385d14b5f64a0e7d52c518";
    sha256 = "0hbsgbqjdyyfn0113h30v4pdlvgl3qrbbp8nzbdp99asnqhxqzam";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase = ''
    install -D -m644 $src/backgrounds/Godafoss_Iceland.jpg \
      $out/share/backgrounds/gnome/Godafoss_Iceland.jpg

    install -D -m644 $src/backgrounds/nixos-border.png \
      $out/share/backgrounds/gnome/nixos-border.png

    install -D -m644 $src/backgrounds/solid-0B1F27.png \
      $out/share/backgrounds/gnome/solid-0B1F27.png

    install -D -m644 $src/backgrounds/solid-112026.png \
      $out/share/backgrounds/gnome/solid-112026.png
  '';

  meta = {
    description = "Nice background images";
  };
}
