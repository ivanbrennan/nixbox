{ abduco
, alacritty
, coreutils
, diss
, dmenu_abduco
, dmenu_cdpath
, dmenu_diss
, util-linux
, stdenv
, runCommandLocal
}:

runCommandLocal "computables" { } ''
  install -D -m755 ${./ao} $out/bin/ao
  patchShebangs --host $out/bin
  substituteInPlace $out/bin/ao                               \
      --subst-var-by "abduco"    "${abduco}/bin/abduco"       \
      --subst-var-by "alacritty" "${alacritty}/bin/alacritty" \
      --subst-var-by "cat"       "${coreutils}/bin/cat"       \
      --subst-var-by "basename"  "${coreutils}/bin/basename"  \
      --subst-var-by "dirname"   "${coreutils}/bin/dirname"   \
      --subst-var-by "setsid"    "${util-linux}/bin/setsid"

  install -D -m755 ${./dmenu_abduco-reattach} $out/bin/dmenu_abduco-reattach
  patchShebangs --host $out/bin
  substituteInPlace $out/bin/dmenu_abduco-reattach                     \
      --subst-var-by "dmenu_abduco" "${dmenu_abduco}/bin/dmenu_abduco" \
      --subst-var-by "alacritty"    "${alacritty}/bin/alacritty"       \
      --subst-var-by "abduco"       "${abduco}/bin/abduco"

  install -D -m755 ${./dmenu_cdpath-alacritty} $out/bin/dmenu_cdpath-alacritty
  patchShebangs --host $out/bin
  substituteInPlace $out/bin/dmenu_cdpath-alacritty                    \
      --subst-var-by "dmenu_cdpath" "${dmenu_cdpath}/bin/dmenu_cdpath" \
      --subst-var-by "cat"          "${coreutils}/bin/cat"             \
      --subst-var-by "alacritty"    "${alacritty}/bin/alacritty"

  install -D -m755 ${./dmenu_diss-reattach} $out/bin/dmenu_diss-reattach
  patchShebangs --host $out/bin
  substituteInPlace $out/bin/dmenu_diss-reattach                 \
      --subst-var-by "dmenu_diss" "${dmenu_diss}/bin/dmenu_diss" \
      --subst-var-by "diss"       "${diss}/bin/diss"

  install -D -m755 ${./dmenu_diss-reattach-terminal} $out/bin/dmenu_diss-reattach-terminal
  patchShebangs --host $out/bin
  substituteInPlace $out/bin/dmenu_diss-reattach-terminal        \
      --subst-var-by "dmenu_diss" "${dmenu_diss}/bin/dmenu_diss" \
      --subst-var-by "diss"       "${diss}/bin/diss"

  ${stdenv.shell} -n $out/bin/ao
  ${stdenv.shell} -n $out/bin/dmenu_abduco-reattach
  ${stdenv.shell} -n $out/bin/dmenu_cdpath-alacritty
  ${stdenv.shell} -n $out/bin/dmenu_diss-reattach
  ${stdenv.shell} -n $out/bin/dmenu_diss-reattach-terminal
''
