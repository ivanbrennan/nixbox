{ pkgs, ... }:

{
  programs = {
    bash = (import ./bash) pkgs;
    tmux = (import ./tmux) pkgs;
  };
}
