{ mpv
, runCommand
, makeWrapper
, dejavu_fonts
}:

let
  input-conf = ./input.conf;
in

runCommand "mpv" {
  buildInputs = [ makeWrapper ];
} ''
  mkdir $out

  ln -s ${mpv}/* $out
  rm $out/bin
  mkdir $out/bin

  makeWrapper ${mpv}/bin/mpv $out/bin/mpv \
      --inherit-argv0 \
      --add-flags "--input-conf=${input-conf}"

  ln -s ${mpv}/bin/mpv_identify.sh $out/bin/mpv_identify.sh

  makeWrapper ${mpv}/bin/umpv $out/bin/umpv \
      --inherit-argv0 \
      --set MPV "mpv --input-conf=${input-conf}"
''
