{ stdenv, fetchFromGitHub, kernel }:

stdenv.mkDerivation rec {
  name = "i8042_debounce-${version}-${kernel.version}";
  version = "0.1";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "i8042-debounce";
    rev = "428ab56aeafb09fb878eb48c2c365d8e758c8879";
    sha256 = "1r3f0cvfzp26812bgcmarccssvzznbv8n4zd3qahc656p5b1hmpb";
  };

  nativeBuildInputs = kernel.moduleBuildDependencies;

  hardeningDisable = [ "pic" ];

  makeFlags = [
    "KVERSION=${kernel.version}"
    "KDIR=${kernel.dev}/lib/modules/${kernel.modDirVersion}/build"
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
