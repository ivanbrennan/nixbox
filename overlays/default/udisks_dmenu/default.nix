{ coreutils
, dmenu
, gawk
, gnused
, jq
, libnotify
, pinentry-gnome3
, runCommandLocal
, stdenv
, udisks2
, util-linux
}:

runCommandLocal "udisks_dmenu" { } ''
  install -D -m755 ${./udisks_dmenu} $out/bin/$name
  patchShebangs --host $out/bin
  substituteInPlace $out/bin/$name                                 \
      --subst-var-by "awk"         "${gawk}/bin/gawk"              \
      --subst-var-by "dmenu"       "${dmenu}/bin/dmenu"            \
      --subst-var-by "jq"          "${jq}/bin/jq"                  \
      --subst-var-by "lsblk"       "${util-linux}/bin/lsblk"       \
      --subst-var-by "notify_send" "${libnotify}/bin/notify-send"  \
      --subst-var-by "pinentry"    "${pinentry-gnome3}/bin/pinentry" \
      --subst-var-by "sed"         "${gnused}/bin/sed"             \
      --subst-var-by "sort"        "${coreutils}/bin/sort"         \
      --subst-var-by "tr"          "${coreutils}/bin/tr"           \
      --subst-var-by "udisksctl"   "${udisks2}/bin/udisksctl"

  ${stdenv.shell} -n $out/bin/$name
''
