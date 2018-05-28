{ pkgs, ... }:

{
  programs = {
    bash = (import ./bash) pkgs;
    less = (import ./less) pkgs;
    ssh  = (import ./ssh);
    tmux = (import ./tmux) pkgs;
  };
}
