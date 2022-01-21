{ pkgs, ... }:

{
  programs = {
    bash = (import ./bash) pkgs;
    dconf.enable = true;
    chromium = (import ./chromium) pkgs;
    gnupg.agent.enable = true;
    less = (import ./less) pkgs;
    light.enable = true;
    nm-applet.enable = true;
    seahorse.enable = true;
    ssh  = (import ./ssh);
    tmux = (import ./tmux) pkgs;
  };
}
