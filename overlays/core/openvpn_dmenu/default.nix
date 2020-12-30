{ coreutils
, dmenu
, gnugrep
, systemd
, writeShellScriptBin
}:

writeShellScriptBin "openvpn_dmenu" ''
  set -u

  active_clients=$(
      ${systemd}/bin/systemctl show openvpn-* \
          --type=service \
          --state=running \
          --property=Id \
          | ${gnugrep}/bin/grep \
              --only-matching \
              --perl-regexp \
              '^Id=\K.+(?=.service$)'
  )

  if [ "''${1-}" = "restart" ]
  then
      restart=true
      shift
  else
      restart=false
  fi

  if [ -n "$active_clients" ]
  then
      if $restart
      then
          unit_command=restart
      else
          unit_command=stop
      fi
      units="$active_clients"
  else
      unit_command=start
      units=$(
          ${systemd}/bin/systemctl list-unit-files openvpn-* \
              --type=service \
              | ${gnugrep}/bin/grep \
                  --only-matching \
                  --perl-regexp \
                  '^openvpn-.+(?=.service)'
      )
  fi

  unit=$(echo "$units" | ${dmenu}/bin/dmenu -p "$unit_command" "$@")
  [ $? -eq 0 ] && ${systemd}/bin/systemctl "$unit_command" "$unit"
''
