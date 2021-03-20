{ writeShellScriptBin }:

writeShellScriptBin "emc" ''
  exec emacsclient --alternate-editor="" --create-frame "$@"
''
