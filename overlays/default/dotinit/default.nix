{ runCommandLocal
, stdenv
}:

runCommandLocal "dotinit" { } ''
  install -D -m755 ${./dotinit} $out/bin/$name
  patchShebangs --host $out/bin
  substituteInPlace $out/bin/$name --subst-var-by "out" "$out"
  ${stdenv.shell} -n $out/bin/$name

  mkdir $out/targets
  cp -aR ${./targets}/. $out/targets
''
