{ pkgs, ... }:

{
  programs = {
    bash = (import ./bash) pkgs;
    less = (import ./less) pkgs;
    light.enable = true;
    ssh  = (import ./ssh);
    tmux = (import ./tmux) pkgs;
  };
}
