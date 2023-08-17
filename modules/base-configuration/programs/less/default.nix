{ pkgs, ... }:

{
  enable = true;
  configFile = "${pkgs.etcdots}/lesskey";
}
