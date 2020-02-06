{ alacritty
, runCommand
, makeWrapper
}:

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
''
