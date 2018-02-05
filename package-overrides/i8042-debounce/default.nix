{ stdenv, fetchFromGitHub, kernel }:

stdenv.mkDerivation rec {
  name = "i8042_debounce-${version}-${kernel.version}";
  version = "0.1";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "i8042-debounce";
    rev = "28c1c01899f2965cff23ca9c99fa17e622c66919";
    sha256 = "1rk6glmx0n2swwcfc9q7mb2afvgz81qy9pbria2npzcqxwgpld3n";
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
