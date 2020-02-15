{ coreutils
, maim
, writeShellScriptBin
, xclip
}:

writeShellScriptBin "screenshot" ''
  set -eu

  all=false
  clip=false

  while getopts "ac" opt
  do
      case "$opt" in
          a) all=true  ;;
          c) clip=true ;;
          *) exit 1    ;;
      esac
  done

  if "$all"
  then
      shoot="${maim}/bin/maim"
  else
      shoot="${maim}/bin/maim --select"
  fi

  if "$clip"
  then
      $shoot | ${xclip}/bin/xclip -selection clipboard -target image/png
  else
      mkdir -p ~/Pictures
      $shoot ~/Pictures/screenshot.$(${coreutils}/bin/date +%s).png
  fi
''
