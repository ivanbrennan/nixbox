{ coreutils
, ffmpeg
, gnugrep
, libnotify
, runCommandLocal
, slop
, stdenv
, xrandr
}:

runCommandLocal "screencast" { } ''
  install -D -m755 ${./screencast} $out/bin/$name
  patchShebangs --host $out/bin
  substituteInPlace $out/bin/$name                           \
      --replace "date"        "${coreutils}/bin/date"        \
      --replace "ffmpeg"      "${ffmpeg}/bin/ffmpeg"         \
      --replace "grep"        "${gnugrep}/bin/grep"          \
      --replace "notify-send" "${libnotify}/bin/notify-send" \
      --replace "slop"        "${slop}/bin/slop"             \
      --replace "xrandr"      "${xrandr}/bin/xrandr"

  ${stdenv.shell} -n $out/bin/$name
''
