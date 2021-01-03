{ coreutils
, dmenu
, findutils
, gnused
, makeWrapper
, stdenv
, runCommandLocal
}:

runCommandLocal "dmenu_cdpath" {
  buildInputs = [ makeWrapper ];
} ''
  install -D -m755 ${./dmenu_cdpath} $out/bin/$name
  patchShebangs --host $out/bin
  substituteInPlace $out/bin/$name                   \
      --subst-var-by "dmenu" "${dmenu}/bin/dmenu"    \
      --subst-var-by "cat"   "${coreutils}/bin/cat"  \
      --subst-var-by "sort"  "${coreutils}/bin/sort" \
      --subst-var-by "find"  "${findutils}/bin/find" \
      --subst-var-by "sed"   "${gnused}/bin/sed"

  ${stdenv.shell} -n $out/bin/$name
''
