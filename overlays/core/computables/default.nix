{ alacritty
, coreutils
, diss
, dmenu_cdpath
, dmenu_diss
, util-linux
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

  install -D -m755 ${./dmenu_diss-reattach-terminal} $out/bin/dmenu_diss-reattach-terminal
  patchShebangs --host $out/bin
  substituteInPlace $out/bin/dmenu_diss-reattach-terminal        \
      --subst-var-by "dmenu_diss" "${dmenu_diss}/bin/dmenu_diss" \
      --subst-var-by "diss"       "${diss}/bin/diss"

  ${stdenv.shell} -n $out/bin/dmenu_cdpath-alacritty
  ${stdenv.shell} -n $out/bin/dmenu_diss-reattach-terminal
''
