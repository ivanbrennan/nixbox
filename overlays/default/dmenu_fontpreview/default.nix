{ coreutils
, dmenu
, findutils
, fontconfig
, gnugrep
, imagemagick
, stdenv
, runCommandLocal
}:

runCommandLocal "dmenu_fontpreview" { } ''
  install -D -m755 ${./dmenu_fontpreview} $out/bin/$name
  patchShebangs --host $out/bin
  substituteInPlace $out/bin/$name                          \
      --subst-var-by "dmenu"   "${dmenu}/bin/dmenu"         \
      --subst-var-by "cat"     "${coreutils}/bin/cat"       \
      --subst-var-by "sort"    "${coreutils}/bin/sort"      \
      --subst-var-by "cut"     "${coreutils}/bin/cut"       \
      --subst-var-by "fc_list" "${fontconfig}/bin/fc-list"  \
      --subst-var-by "grep"    "${gnugrep}/bin/grep"        \
      --subst-var-by "display" "${imagemagick}/bin/display" \
      --subst-var-by "xargs"   "${findutils}/bin/xargs"

  ${stdenv.shell} -n $out/bin/$name
''
