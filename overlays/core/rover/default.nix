{ stdenv
, lib
, fetchurl
, zlib
, autoPatchelfHook
, callPackage
}:

stdenv.mkDerivation rec {
  pname = "rover";

  version = "0.5.1";

  src = fetchurl {
    url = "https://github.com/apollographql/rover/releases/download/v${version}/rover-v${version}-x86_64-unknown-linux-gnu.tar.gz";
    sha256 = "sha256-BJOjL+mlnniQNt2Z+qBA5prO7md2rj19FojVqakhQ9Q=";
  };

  nativeBuildInputs = [ autoPatchelfHook ];

  buildInputs = [ zlib ];

  sourceRoot = ".";

  installPhase = ''
    install -m755 -D dist/rover $out/bin/rover
  '';
}
