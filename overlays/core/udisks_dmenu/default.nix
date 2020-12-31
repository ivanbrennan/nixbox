{ coreutils
, dmenu
, gawk
, libnotify
, runCommandLocal
, stdenv
, udisks2
, util-linux
}:

runCommandLocal "udisks_dmenu" { } ''
  install -D -m755 ${./udisks_dmenu} $out/bin/$name
  patchShebangs --host $out/bin
  substituteInPlace $out/bin/$name                           \
      --replace "awk"         "${gawk}/bin/gawk"             \
      --replace "dmenu"       "${dmenu}/bin/dmenu"           \
      --replace "lsblk"       "${util-linux}/bin/lsblk"      \
      --replace "notify-send" "${libnotify}/bin/notify-send" \
      --replace "udisks"      "${udisks2}/bin/udisks"

  ${stdenv.shell} -n $out/bin/$name
''
