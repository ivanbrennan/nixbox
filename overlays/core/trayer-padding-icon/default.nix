{ coreutils
, gnugrep
, xorg
, stdenv
, runCommandLocal
}:

runCommandLocal "trayer-padding-icon" { } ''
  install -D -m755 ${./trayer-padding-icon} $out/bin/$name
  patchShebangs --host $out/bin
  substituteInPlace $out/bin/$name                     \
      --subst-var-by "cat"   "${coreutils}/bin/cat"    \
      --subst-var-by "seq"   "${coreutils}/bin/seq"    \
      --subst-var-by "grep"  "${gnugrep}/bin/grep"     \
      --subst-var-by "xprop" "${xorg.xprop}/bin/xprop"

  ${stdenv.shell} -n $out/bin/$name
''
