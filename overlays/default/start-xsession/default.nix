{ kbd
, runCommandLocal
, stdenv
, writeShellScriptBin
, xauth
, xinit
, xorg
, xterm
}:

let
  startx-minimal = runCommandLocal "startx-minimal" { } ''
    install -D -m755 ${./startx-minimal} $out/bin/$name
    patchShebangs --host $out/bin
    substituteInPlace $out/bin/$name                          \
        --subst-var-by "xauth"     "${xorg.xauth}/bin/xauth"  \
        --subst-var-by "xinit"     "${xorg.xinit}/bin/xinit"  \
        --subst-var-by "xserver"   "${xorg.xorgserver}/bin/X" \
        --subst-var-by "xterm"     "${xterm}/bin/xterm"       \
        --subst-var-by "deallocvt" "${kbd}/bin/deallocvt"

    ${stdenv.shell} -n $out/bin/$name
  '';

in writeShellScriptBin "start-xsession" ''
  exec /run/current-system/systemd/bin/systemd-cat \
      --identifier=startx \
      -- ${startx-minimal}/bin/startx-minimal
''
