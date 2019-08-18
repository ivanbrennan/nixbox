{ pkgs, ... }:

{
  programs = {
    bash = (import ./bash) pkgs;
    gpaste.enable = true;
    less = (import ./less) pkgs;
    ssh  = (import ./ssh);
    tmux = (import ./tmux) pkgs;
  };
}
