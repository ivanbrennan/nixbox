{ nsxiv,
  runCommandLocal,
  symlinkJoin,
  writeShellScriptBin
}:

let
  configured = nsxiv.override {
    conf = builtins.readFile ./config.h;
  };

  nsxiv-wrapped = writeShellScriptBin "nsxiv" ''
    NSXIV_OPTS=''${NSXIV_OPTS:-"--animate --no-bar"}
    exec ${configured}/bin/nsxiv $NSXIV_OPTS "$@"
  '';

  nsxiv-unwrapped = runCommandLocal "nsxiv-unwrapped" { } ''
    install -D -m755 ${configured}/bin/nsxiv $out/bin/nsxiv-unwrapped
  '';

in symlinkJoin {
  name = "nsxiv";
  paths = [
    nsxiv-wrapped
    nsxiv-unwrapped
    nsxiv.man
    nsxiv.doc
  ];
}
