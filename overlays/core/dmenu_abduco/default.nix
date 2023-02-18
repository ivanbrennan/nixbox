{ abduco
, coreutils
, dmenu
, gawk
, stdenv
, runCommandLocal
}:

runCommandLocal "dmenu_abduco" { } ''
  install -D -m755 ${./dmenu_abduco} $out/bin/$name
  patchShebangs --host $out/bin
  substituteInPlace $out/bin/$name                   \
      --subst-var-by "abduco" "${abduco}/bin/abduco" \
      --subst-var-by "awk"    "${gawk}/bin/gawk"     \
      --subst-var-by "dmenu"  "${dmenu}/bin/dmenu"   \
      --subst-var-by "sort"   "${coreutils}/bin/sort"

  ${stdenv.shell} -n $out/bin/$name
''
