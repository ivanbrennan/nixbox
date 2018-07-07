#! /usr/bin/env nix-shell
#! nix-shell -i bash -p gnused coreutils jq

set -eu

if [ $# != 2 ]; then
    printf '\n%sUsage%s:\n%s %sURL REVISION%s\n' \
        $(tput bold) $(tput sgr0) ${0} $(tput sitm) $(tput sgr0)
    printf '\n%sExample%s:\n%s git@github.com:SumAll/sumall-cli.git 7.5.0\n' \
        $(tput bold) $(tput sgr0) ${0}
    exit 1
fi

url=$1
rev=$2

version_nix=$(dirname $0)/version.nix

pat='^\(\s*rev\s*=\s*"\)[^"]\+'
sed -i "s|${pat}|\\1${rev}|" $version_nix

pat='^\(\s*sha256\s*=\s*"\)[^"]\+'
sha256=$(nix-prefetch-git --quiet $url $rev | jq -r '.sha256')
sed -i "s|${pat}|\\1${sha256}|" $version_nix
