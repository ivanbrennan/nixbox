{ pkgs, ... }:

{
  enable = true;
  extraTmuxConf = builtins.readFile "${pkgs.etcdots}/etc/tmux.conf";
}
