{ pkgs }:

let
  python = import ../sumall-cli/requirements.nix { inherit pkgs; };
in
  python.packages."argcomplete"
