{ coreutils
, ffmpeg-full
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
  substituteInPlace $out/bin/$name                                \
      --replace-fail "date"        "${coreutils}/bin/date"        \
      --replace-fail "ffmpeg"      "${ffmpeg-full}/bin/ffmpeg"    \
      --replace-fail "grep"        "${gnugrep}/bin/grep"          \
      --replace-fail "notify-send" "${libnotify}/bin/notify-send" \
      --replace-fail "slop"        "${slop}/bin/slop"             \
      --replace-fail "xrandr"      "${xrandr}/bin/xrandr"

  ${stdenv.shell} -n $out/bin/$name
''
