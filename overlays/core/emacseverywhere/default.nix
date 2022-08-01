{ writeShellScriptBin }:

writeShellScriptBin "emacseverywhere" ''
  exec emacsclient --alternate-editor="" --eval "(emacs-everywhere)" "$@"
''
