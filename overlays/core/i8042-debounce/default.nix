{ stdenv, fetchFromGitHub, kernel, lib }:

stdenv.mkDerivation rec {
  name = "i8042_debounce-${version}-${kernel.version}";
  version = "0.1";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "i8042-debounce";
    rev = "fbccddfb6101afc8da28a3be0b6ee8d7107252fe";
    sha256 = "1l0q13h4wh03bb89499p6dr0fvzn7flsmgpaj1scrb2kchj28gr7";
  };

  nativeBuildInputs = kernel.moduleBuildDependencies;

  hardeningDisable = [ "pic" ];

  makeFlags = [
    "KVERSION=${kernel.version}"
    "KDIR=${kernel.dev}/lib/modules/${kernel.modDirVersion}/build"
  ];

  dontPatchELF = true;

  installPhase = ''
    install -D -m644 i8042_debounce.ko \
      $out/lib/modules/${kernel.modDirVersion}/misc/i8042_debounce/i8042_debounce.ko
  '';

  postInstall = ''
    xz $out/lib/modules/${kernel.modDirVersion}/misc/i8042_debounce/i8042_debounce.ko
  '';

  enableParallelBuilding = true;

  meta = {
    description = "Module to debounce keypresses on crapppyy keyboards";
    homepage = https://github.com/nylen/i8042-debounce;
    license = lib.licenses.gpl3;
    platforms = [ "x86_64-linux" ];
  };
}
