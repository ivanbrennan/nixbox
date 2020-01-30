{ stdenv
, fetchFromGitHub
}:

stdenv.mkDerivation {
  name = "nice-backgrounds";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "dotfiles";
    rev = "9cbc16833666308ba8f88c3448e0529feb7390af";
    sha256 = "0n2fkzsaqnlvadpy830xkg9ng16gz31ic99ghl5ai4n4bhqwvvc6";
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
  '';

  meta = {
    description = "Nice background images";
  };
}
