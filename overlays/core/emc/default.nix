{ writeShellScriptBin }:

writeShellScriptBin "emc" ''
  exec emacsclient -c "$@"
''
