{ stdenv, fetchFromGitHub, kernel }:

stdenv.mkDerivation rec {
  name = "i8042_debounce-${version}-${kernel.version}";
  version = "0.1";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "i8042-debounce";
    rev = "27f9f355b004c69f70323615ff37c4795fb6cb7b";
    sha256 = "1wyngkbwx9s6nm1vc65948vyrwf4a9hnww2x9szlhrj5m6hrj78m";
  };

  nativeBuildInputs = kernel.moduleBuildDependencies;

  hardeningDisable = [ "pic" ];

  makeFlags = [
    "KVERSION=${kernel.version}"
    "KDIR=${kernel.dev}/lib/modules/${kernel.modDirVersion}/build"
    "MULTI_THRESHOLD=90"
    "SINGLE_THRESHOLD=60"
  ];

  dontPatchELF = true;

  installPhase = ''
    mkdir -p $out/lib/modules/${kernel.modDirVersion}/misc/i8042_debounce
    install i8042_debounce.ko $out/lib/modules/${kernel.modDirVersion}/misc/i8042_debounce
  '';

  enableParallelBuilding = true;

  meta = {
    description = "Module to debounce keypresses on crapppyy keyboards";
    homepage = https://github.com/ivanbrennan/i8042-debounce;
    license = stdenv.lib.licenses.gpl3;
    platforms = [ "x86_64-linux" ];
  };
}
