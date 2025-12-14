{ pkgs, ... }:

{
  enable = true;
  configFile = "${pkgs.etcdots}/lesskey";
  lessopen = "|${pkgs.lesspipe}/bin/lesspipe.sh %s";
}
