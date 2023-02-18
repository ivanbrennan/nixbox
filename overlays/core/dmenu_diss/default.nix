{ diss
, coreutils
, dmenu
, stdenv
, runCommandLocal
}:

runCommandLocal "dmenu_diss" { } ''
  install -D -m755 ${./dmenu_diss} $out/bin/$name
  patchShebangs --host $out/bin
  substituteInPlace $out/bin/$name                   \
      --subst-var-by "diss"   "${diss}/bin/diss"     \
      --subst-var-by "dmenu"  "${dmenu}/bin/dmenu"   \
      --subst-var-by "sort"   "${coreutils}/bin/sort"

  ${stdenv.shell} -n $out/bin/$name
''
