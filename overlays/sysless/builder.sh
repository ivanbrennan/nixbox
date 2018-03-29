source $stdenv/setup

PATH=$less/bin:$PATH

mkdir -p $out/etc

lesskey --output=$out/etc/sysless -- $src/lesskey
