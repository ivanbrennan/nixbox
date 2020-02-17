{ stdenv
, fetchFromGitHub
}:

stdenv.mkDerivation {
  name = "nice-backgrounds";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "dotfiles";
    rev = "44b05f646011a9aea60d7b5ef66f973add9bda6c";
    sha256 = "0p1yhb63cmz4qddydlrbqdavx0c9kr7bvknywak71r490pnf7h55";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase = ''
    install -D -m644 $src/backgrounds/Godafoss_Iceland.jpg \
      $out/share/backgrounds/gnome/Godafoss_Iceland.jpg

    install -D -m644 $src/backgrounds/snow-and-sky.jpg \
      $out/share/backgrounds/gnome/snow-and-sky.jpg

    install -D -m644 $src/backgrounds/solid-0B1F27.png \
      $out/share/backgrounds/gnome/solid-0B1F27.png

    install -D -m644 $src/backgrounds/solid-112026.png \
      $out/share/backgrounds/gnome/solid-112026.png
  '';

  meta = {
    description = "Nice background images";
  };
}
