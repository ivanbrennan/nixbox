{ pkgs, ... }:

{
  programs = {
    bash = (import ./bash) pkgs;
    less = (import ./less) pkgs;
    tmux = (import ./tmux) pkgs;
  };
}
