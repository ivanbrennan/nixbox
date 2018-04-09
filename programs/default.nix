{ pkgs, ... }:

{
  programs = {
    bash = import ./bash;
    tmux = (import ./tmux) pkgs;
    less = (import ./less) pkgs;
  };
}
