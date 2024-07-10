{ coreutils
, findutils
, gnugrep
, nsxiv
, runCommandLocal
, stdenv
, symlinkJoin
, writeShellScriptBin
}:

let
  configured = nsxiv.override {
    conf = builtins.readFile ./config.h;
  };

  nsxiv-unwrapped = runCommandLocal "nsxiv-unwrapped" { } ''
    install -D -m755 ${configured}/bin/nsxiv $out/bin/nsxiv-unwrapped
  '';

  nsxiv-wrapped = writeShellScriptBin "nsxiv" ''
    NSXIV_OPTS=''${NSXIV_OPTS:-"--animate --no-bar"}
    exec ${configured}/bin/nsxiv $NSXIV_OPTS "$@"
  '';

  nsxiv-rifle = runCommandLocal "nsxiv-rifle" { } ''
    install -D -m755 ${./nsxiv-rifle} $out/bin/$name
    patchShebangs --host $out/bin
    substituteInPlace $out/bin/$name                        \
        --subst-var-by "grep"  "${gnugrep}/bin/grep"        \
        --subst-var-by "find"  "${findutils}/bin/find"      \
        --subst-var-by "nsxiv" "${nsxiv-wrapped}/bin/nsxiv" \
        --subst-var-by "sort"  "${coreutils}/bin/sort"      \
        --subst-var-by "tee"   "${coreutils}/bin/tee"
    ${stdenv.shell} -n $out/bin/$name
  '';

in symlinkJoin {
  name = "nsxiv";
  paths = [
    nsxiv-unwrapped
    nsxiv-wrapped
    nsxiv-rifle
    nsxiv.man
    nsxiv.doc
  ];
}
