{ runCommandLocal
, stdenv
, systemd
, xdotool
}:

runCommandLocal "resound" { } ''
  install -D -m755 ${./resound} $out/bin/resound
  install -D -m755 ${./remod-sof} $out/bin/remod-sof

  patchShebangs --host $out/bin

  substituteInPlace $out/bin/resound \
      --replace-fail "systemctl" "${systemd}/bin/systemctl" \
      --replace-fail "xdotool"   "${xdotool}/bin/xdotool"

  ${stdenv.shell} -n $out/bin/resound
  ${stdenv.shell} -n $out/bin/remod-sof
''
