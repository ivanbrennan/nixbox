{ writeShellScriptBin, emacs }:

writeShellScriptBin "emc" ''
  exec ${emacs}/bin/emacsclient --alternate-editor="" --create-frame "$@"
''
