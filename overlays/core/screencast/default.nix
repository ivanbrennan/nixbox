{ coreutils
, ffmpeg
, gnugrep
, slop
, writeShellScriptBin
, xrandr
}:

# TODO: notify-send
writeShellScriptBin "screencast" ''
  set -u

  expectedExitCode=0
  checkError() {
      local exitCode=$?
      if [ "$exitCode" -eq "$expectedExitCode" ]
      then
          expectedExitCode=0
      else
          exit "$exitCode"
      fi
  }
  trap "checkError" ERR

  exec > ~/.screencast.log 2>&1

  all=false

  while getopts "a" opt
  do
      case "$opt" in
          a) all=true ;;
          *) exit 1   ;;
      esac
  done

  tmpfile="$(mktemp -t screencast-XXXXXXX)"
  palette="$(mktemp -t palette-XXXXXXX)"
  tmpfile_mkv="''${tmpfile}.mkv"
  palette_png="''${palette}.png"

  cleanup() {
      rm -f "$tmpfile" "$tmpfile_mkv" "$palette" "$palette_png"
  }
  trap "cleanup" EXIT QUIT TERM

  mkdir -p "$HOME/Pictures"
  output="$HOME/Pictures/screencast.$(${coreutils}/bin/date +%s)"

  screenRegion() {
      ${xrandr}/bin/xrandr \
          | ${gnugrep}/bin/grep \
                --max-count=1 \
                --only-matching \
                --extended-regexp \
                '[0-9]+x[0-9]+\+[0-9]+\+[0-9]+' \
          | tr x+ ' '
  }

  selectRegion() {
      ${slop}/bin/slop --format "%w %h %x %y"
  }

  if "$all"
  then
      region=$(screenRegion)
  else
      region=$(selectRegion)
  fi

  read -r W H X Y < <(echo "$region")

  # External process may stop the recording via signal (e.g. "killall ffmpeg"),
  # in which case ffmpeg exits with code 255.
  expectedExitCode=255
  ${ffmpeg}/bin/ffmpeg -y -f x11grab -s "$W"x"$H" -i :0.0+$X,$Y "$tmpfile_mkv"
  expectedExitCode=0

  # notify-send 'generating palette'
  ${ffmpeg}/bin/ffmpeg -y -i "$tmpfile_mkv" -vf fps=10,palettegen "$palette_png"

  # notify-send 'generating gif'
  ${ffmpeg}/bin/ffmpeg -y -i "$tmpfile_mkv" -i "$palette_png" -filter_complex "paletteuse" $output.gif

  mv $tmpfile_mkv $output.mkv
  echo "$output"

  # notify-send "size $(du -h $output.gif | awk '{print $1}')"
''
