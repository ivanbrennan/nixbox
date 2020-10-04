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
  trap checkError ERR

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

  exec > ~/.screencast.log 2>&1

  all=false
  while getopts "a" opt
  do
      case "$opt" in
          a) all=true ;;
          *) exit 1   ;;
      esac
  done

  tmpfile="$(mktemp -t screencast-XXXXXXX.mkv)"
  palette="$(mktemp -t palette-XXXXXXX.png)"
  trap "rm -f '$tmpfile' '$palette'" EXIT QUIT TERM

  mkdir -p "$HOME/Pictures"
  output="$HOME/Pictures/screencast.$(${coreutils}/bin/date +%s)"

  if "$all"
  then
      region=$(screenRegion)
  else
      region=$(selectRegion)
  fi

  read -r W H X Y < <(echo "$region")

  # Recording may be stopped by running "killall ffmpeg",
  # in which case ffmpeg exits with code 255.
  #
  expectedExitCode=255
  ${ffmpeg}/bin/ffmpeg -y -f x11grab -s "$W"x"$H" -i :0.0+"$X","$Y" "$tmpfile"
  expectedExitCode=0

  # notify-send 'generating palette'
  ${ffmpeg}/bin/ffmpeg -y -i "$tmpfile" -vf fps=10,palettegen "$palette"

  # notify-send 'generating gif'
  ${ffmpeg}/bin/ffmpeg -y -i "$tmpfile" -i "$palette" -filter_complex "paletteuse" "$output".gif

  mv "$tmpfile" "$output".mkv
  echo "$output"

  # notify-send "size $(du -h $output.gif | awk '{print $1}')"
''
