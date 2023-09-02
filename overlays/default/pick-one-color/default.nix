{ writeShellScriptBin
, gpick
, xsel
}:

writeShellScriptBin "pick-one-color" ''
  set -eu

  color=$(${gpick}/bin/gpick --single --output --no-newline)

  if [ -n "$color" ]
  then
      echo -n "$color" | ${xsel}/bin/xsel --clipboard
  fi
''
