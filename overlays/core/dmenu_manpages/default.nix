{ coreutils
, dmenu
, man
, perl
, stdenv
, runCommandLocal
}:

runCommandLocal "dmenu_manpages" { } ''
  install -D -m755 ${./dmenu_manpages} $out/bin/$name
  patchShebangs --host $out/bin
  substituteInPlace $out/bin/$name                     \
      --subst-var-by "dmenu"   "${dmenu}/bin/dmenu"    \
      --subst-var-by "ls"      "${coreutils}/bin/ls"   \
      --subst-var-by "manpath" "${man}/bin/manpath"    \
      --subst-var-by "perl"    "${perl}/bin/perl"      \
      --subst-var-by "sort"    "${coreutils}/bin/sort"

  ${stdenv.shell} -n $out/bin/$name
''
