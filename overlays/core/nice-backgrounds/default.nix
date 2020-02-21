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
    images=(
      Godafoss_Iceland.jpg
      lookup.jpg
      moscow-subway.jpg
      mountains.jpg
      owl-eye.jpg
      snow-and-sky.jpg
      solid-0B1F27.png
      solid-112026.png
    )

    for img in ''${images[@]}
    do
        install -D -m644 $src/backgrounds/$img \
          $out/share/backgrounds/gnome/$img
    done
  '';

  meta = {
    description = "Nice background images";
  };
}
