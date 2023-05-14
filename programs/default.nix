{ pkgs, ... }:

{
  programs = {
    bash = (import ./bash) pkgs;
    dconf.enable = true;
    gnupg.agent.enable = true;
    less = (import ./less) pkgs;
    light.enable = true;
    nm-applet.enable = true;
    seahorse.enable = true;
    ssh  = (import ./ssh);
    tmux = (import ./tmux) pkgs;
    wireshark = {
      enable = true;
      package = pkgs.wireshark;
    };
  };
}
