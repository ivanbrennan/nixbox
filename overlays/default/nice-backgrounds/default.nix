{ stdenv
, fetchFromGitHub
}:

stdenv.mkDerivation {
  name = "nice-backgrounds";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "dotfiles";
    rev = "f12677a68fbae48d29db5251e88adfe5798336fd";
    sha256 = "sha256-K/CsjWJBVYY3zfzQZarlvTDBbL87Rfm5AiZjNSXoo5o=";
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
      nix-wallpaper-binary-black_8k.png
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
