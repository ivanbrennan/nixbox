{ alacritty
, gnused
, runCommand
, makeWrapper
}:

let
  transparentConfig = runCommand "alacritty-transparent.yml" {
    buildInputs = [ gnused ];
  } ''
    sed 's/opacity: .*/opacity: 0.9/' ${./alacritty.yml} \
      > $out
  '';
in

runCommand "alacritty" {
  buildInputs = [ makeWrapper ];
} ''
  mkdir $out

  ln -s ${alacritty}/* $out
  rm $out/bin
  mkdir $out/bin
  ln -s ${alacritty}/bin/* $out/bin
  mv $out/bin/alacritty $out/bin/alacritty-unwrapped

  makeWrapper ${alacritty}/bin/alacritty $out/bin/alacritty \
    --add-flags "--config-file ${./alacritty.yml}"

  makeWrapper ${alacritty}/bin/alacritty $out/bin/alacritty-transparent \
    --add-flags "--config-file ${transparentConfig}"
''
