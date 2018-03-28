source $stdenv/setup

mkdir -p $out/bin
mkdir -p $out/share/bash-completion/completions

cp $src/vln $out/bin/vln
cp $src/completions/vln $out/share/bash-completion/completions/vln
