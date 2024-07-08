{ coreutils
, dmenu
, gnugrep
, stdenv
, systemd
, runCommandLocal
}:

runCommandLocal "openvpn_dmenu" { } ''
  install -D -m755 ${./openvpn_dmenu} $out/bin/$name
  patchShebangs --host $out/bin
  substituteInPlace $out/bin/$name \
      --replace-fail "systemctl" "${systemd}/bin/systemctl" \
      --replace-fail "grep"      "${gnugrep}/bin/grep" \
      --replace-fail "dmenu"     "${dmenu}/bin/dmenu"

  ${stdenv.shell} -n $out/bin/$name
''
