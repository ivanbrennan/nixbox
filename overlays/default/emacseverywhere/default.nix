{ writeShellScriptBin, emacs }:

writeShellScriptBin "emacseverywhere" ''
  exec ${emacs}/bin/emacsclient --alternate-editor="" --eval "(emacs-everywhere)" "$@"
''
