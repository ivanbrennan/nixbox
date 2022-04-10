{ stdenv
, lib
, fetchurl
, zlib
, autoPatchelfHook
, callPackage
}:

let
  rover-fed2 = callPackage ./rover-fed2.nix { };
in
  stdenv.mkDerivation rec {
    pname = "rover";

    version = "0.4.8";

    src = fetchurl {
      url = "https://github.com/apollographql/rover/releases/download/v${version}/rover-v${version}-x86_64-unknown-linux-gnu.tar.gz";
      sha256 = "sha256-GXKhcePkolzDuMArJDpMjX0W5mpjsU7+3lmzWpJCRcI=";
    };

    nativeBuildInputs = [ autoPatchelfHook ];

    buildInputs = [ zlib ];

    sourceRoot = ".";

    installPhase = ''
      install -m755 -D dist/rover $out/bin/rover
      ln -s ${rover-fed2}/bin/rover-fed2 $out/bin/rover-fed2
    '';
  }
