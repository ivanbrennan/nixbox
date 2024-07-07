{ alacritty
, gnused
, runCommand
, makeWrapper
}:

let
  transparentConfig = runCommand "alacritty-transparent.toml" {
    buildInputs = [ gnused ];
  } ''
    sed 's/opacity = .*/opacity = 0.9/' ${./alacritty.toml} > $out
  '';

  greyConfig = runCommand "alacritty-grey.toml" {
    buildInputs = [ gnused ];
  } ''
    sed 's/"#181818"/"#212226"/' ${./alacritty.toml} > $out
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
    --add-flags "--config-file ${./alacritty.toml}"

  makeWrapper ${alacritty}/bin/alacritty $out/bin/alacritty-transparent \
    --add-flags "--config-file ${transparentConfig}"

  makeWrapper ${alacritty}/bin/alacritty $out/bin/alacritty-grey \
    --add-flags "--config-file ${greyConfig}"
''
