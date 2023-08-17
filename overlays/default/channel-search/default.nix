{ coreutils
, curl
, git
, stdenv
, runCommandLocal
}:

runCommandLocal "channel-search" { } ''
  install -D -m755 ${./channel-search} $out/bin/$name
  patchShebangs --host $out/bin
  substituteInPlace $out/bin/$name                 \
      --subst-var-by "cat"  "${coreutils}/bin/cat" \
      --subst-var-by "curl" "${curl}/bin/curl"     \
      --subst-var-by "cut"  "${coreutils}/bin/cut" \
      --subst-var-by "git"  "${git}/bin/git"

  ${stdenv.shell} -n $out/bin/$name
''
