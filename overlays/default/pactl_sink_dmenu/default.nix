{ coreutils
, dmenu
, jq
, libnotify
, pulseaudio
, runCommandLocal
, stdenv
}:

runCommandLocal "pactl_sink_dmenu" { } ''
  install -D -m755 ${./pactl_sink_dmenu} $out/bin/$name
  patchShebangs --host $out/bin
  substituteInPlace $out/bin/$name                                \
      --subst-var-by "dmenu"       "${dmenu}/bin/dmenu"           \
      --subst-var-by "jq"          "${jq}/bin/jq"                 \
      --subst-var-by "notify_send" "${libnotify}/bin/notify-send" \
      --subst-var-by "pactl"       "${pulseaudio}/bin/pactl"      \
      --subst-var-by "tac"         "${coreutils}/bin/tac"

  ${stdenv.shell} -n $out/bin/$name
''
