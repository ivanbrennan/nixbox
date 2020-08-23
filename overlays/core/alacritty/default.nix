{ alacritty
, gnused
, runCommand
, makeWrapper
}:

let
  transparentConfig = runCommand "alacritty-transparent.yml" {
    buildInputs = [ gnused ];
  } ''
    sed 's/background_opacity: .*/background_opacity: 0.7/' ${./alacritty.yml} \
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
  rm $out/bin/alacritty

  makeWrapper ${alacritty}/bin/alacritty $out/bin/alacritty \
    --add-flags "--config-file ${./alacritty.yml}"

  makeWrapper ${alacritty}/bin/alacritty $out/bin/alacritty-transparent \
    --add-flags "--config-file ${transparentConfig}"
''
