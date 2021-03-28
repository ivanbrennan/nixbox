{ alacritty
, coreutils
, dmenu_cdpath
, dmenu_manpages
, man
, perl
, stdenv
, runCommandLocal
}:

runCommandLocal "computables" { } ''
  install -D -m755 ${./dmenu_cdpath-alacritty} $out/bin/dmenu_cdpath-alacritty
  install -D -m755 ${./dmenu_manpages-alacritty} $out/bin/dmenu_manpages-alacritty

  patchShebangs --host $out/bin

  substituteInPlace $out/bin/dmenu_cdpath-alacritty                    \
      --subst-var-by "dmenu_cdpath" "${dmenu_cdpath}/bin/dmenu_cdpath" \
      --subst-var-by "cat"          "${coreutils}/bin/cat"             \
      --subst-var-by "alacritty"    "${alacritty}/bin/alacritty"

  substituteInPlace $out/bin/dmenu_manpages-alacritty                        \
      --subst-var-by "dmenu_manpages" "${dmenu_manpages}/bin/dmenu_manpages" \
      --subst-var-by "alacritty"      "${alacritty}/bin/alacritty"

  ${stdenv.shell} -n $out/bin/dmenu_cdpath-alacritty
  ${stdenv.shell} -n $out/bin/dmenu_manpages-alacritty
''
