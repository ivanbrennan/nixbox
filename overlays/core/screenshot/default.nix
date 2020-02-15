{ coreutils
, imagemagick
, slop
, writeShellScriptBin
, xclip
}:

writeShellScriptBin "screenshot" ''
  set -eu

  if [[ "''${1-}" == 'root' ]]
  then
      crop=""
  else
      slop=$(${slop}/bin/slop --format="%g") || exit 1
      read -r G < <(echo $slop)
      crop="-crop $G"
  fi

  if [[ "''${1-}" == 'clip' ]]
  then
      ${imagemagick}/bin/import \
          -window root \
          $crop \
          png:- \
          | ${xclip}/bin/xclip -selection clipboard -target image/png
  else
      mkdir -p ~/Pictures
      ${imagemagick}/bin/import \
          -window root \
          $crop \
          ~/Pictures/screenshot_$(${coreutils}/bin/date +%F_%T).png
  fi
''
