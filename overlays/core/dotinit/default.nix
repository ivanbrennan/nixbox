{ runCommandLocal
, stdenv
, stow
}:

runCommandLocal "dotinit" { } ''
  install -D -m755 ${./dotinit} $out/bin/$name
  patchShebangs --host $out/bin
  substituteInPlace $out/bin/$name \
      --subst-var-by "out" "$out" \
      --subst-var-by "stow" "${stow}/bin/stow"

  ${stdenv.shell} -n $out/bin/$name

  mkdir $out/stow
  cp -aR ${./stow}/. $out/stow
''
