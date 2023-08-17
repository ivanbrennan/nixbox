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
      --replace "systemctl" "${systemd}/bin/systemctl" \
      --replace "grep"      "${gnugrep}/bin/grep" \
      --replace "dmenu"     "${dmenu}/bin/dmenu"

  ${stdenv.shell} -n $out/bin/$name
''
