{ pkgs, ... }:

{
  enable = true;
  extraConfig = builtins.readFile "${pkgs.etcdots}/etc/tmux.conf";
}
