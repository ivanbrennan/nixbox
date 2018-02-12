{ stdenv, fetchFromGitHub, kernel }:

stdenv.mkDerivation rec {
  name = "i8042_debounce-${version}-${kernel.version}";
  version = "0.1";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "i8042-debounce";
    rev = "637286367fa1e6993fcb944d4a43ff6a1886af43";
    sha256 = "11dj8lc9i6qqd4znx53dxy1n2qln7jmbvzz97jb7dz61r04by89c";
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
