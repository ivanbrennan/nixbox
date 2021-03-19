{ writeShellScriptBin }:

writeShellScriptBin "emc" ''
  exec emacsclient -a "" -c "$@"
''
