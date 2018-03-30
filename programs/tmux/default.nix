{ pkgs, ... }:

{
  enable = true;
  extraTmuxConf = builtins.readFile "${pkgs.etconfig}/etc/tmux.conf";
}
