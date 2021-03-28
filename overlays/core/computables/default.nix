{ alacritty
, coreutils
, dmenu_cdpath
, stdenv
, runCommandLocal
}:

runCommandLocal "computables" { } ''
  install -D -m755 ${./dmenu_cdpath-alacritty} $out/bin/dmenu_cdpath-alacritty
  patchShebangs --host $out/bin
  substituteInPlace $out/bin/dmenu_cdpath-alacritty                    \
      --subst-var-by "dmenu_cdpath" "${dmenu_cdpath}/bin/dmenu_cdpath" \
      --subst-var-by "cat"          "${coreutils}/bin/cat"             \
      --subst-var-by "alacritty"    "${alacritty}/bin/alacritty"

  ${stdenv.shell} -n $out/bin/dmenu_cdpath-alacritty
''
