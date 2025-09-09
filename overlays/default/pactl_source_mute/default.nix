{ pulseaudio
, runCommandLocal
, stdenv
}:

runCommandLocal "pactl_source_mute" { } ''
  install -D -m755 ${./pactl_source_mute} $out/bin/$name
  patchShebangs --host $out/bin
  substituteInPlace $out/bin/$name \
      --subst-var-by "pactl" "${pulseaudio}/bin/pactl"

  ${stdenv.shell} -n $out/bin/$name
''
