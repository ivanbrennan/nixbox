#! /usr/bin/env nix-shell
#! nix-shell -i bash -p gnused coreutils

set -eu

usage() {
    printf '\n%sUsage%s:\n%s %s[REVISION]%s\n' \
        $(tput bold) $(tput sgr0) ${0} $(tput sitm) $(tput sgr0)
    example
}

example() {
    printf '\n%sExample%s:\n%s 5da85431fb1df4fb3ac36730b2591ccc9bdf5c21\n' \
        $(tput bold) $(tput sgr0) ${0}
}

if (( $# == 1 ))
then
    rev=$1
elif (( $# == 0 ))
then
    pkgs=$(grep -oP 'nixpkgs=\K[^:]+' <<< $NIX_PATH || true)
    rev=$(cat ${pkgs}/.git-revision 2>/dev/null || true)
else
    usage
    exit 1
fi

if [ -z "$rev" ]; then
    printf '\nNo revision found. Try specifying %sREVISION%s\n' $(tput sitm) $(tput sgr0)
    example
    exit 1
fi

nixpkgs=$(dirname $0)/nixpkgs.nix

pat='^\(\s*url\s*=\s*"\)https://github.com/NixOS/nixpkgs/archive/[0-9a-f]\{40\}\.tar\.gz'
url="https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz"
sed -i "s|${pat}|\\1${url}|" $nixpkgs

pat='^\(\s*sha256\s*=\s*"\)[^"]\+'
sha256=$(nix-prefetch-url --unpack ${url})
sed -i "s|${pat}|\\1${sha256}|" $nixpkgs
