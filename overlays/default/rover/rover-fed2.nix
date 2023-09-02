{ stdenv
, lib
, fetchurl
, zlib
, autoPatchelfHook
}:

stdenv.mkDerivation rec {
  pname = "rover-fed2";

  version = "0.4.8";

  src = fetchurl {
    url = "https://github.com/apollographql/rover/releases/download/v${version}/rover-fed2-v${version}-x86_64-unknown-linux-gnu.tar.gz";
    sha256 = "sha256-TbJbqoPdETNzATgopkVQbJgjpfYbW+JDu8PIrqYHDJM==";
  };

  nativeBuildInputs = [ autoPatchelfHook ];

  buildInputs = [ zlib ];

  sourceRoot = ".";

  installPhase = ''
    install -m755 -D dist/rover-fed2 $out/bin/rover-fed2
  '';
}
