{ stdenv
, runCommandLocal
}:

runCommandLocal "dmenu_pass_otp" { } ''
  install -D -m755 ${./dmenu_pass_otp} $out/bin/$name
  patchShebangs --host $out/bin

  ${stdenv.shell} -n $out/bin/$name
''
