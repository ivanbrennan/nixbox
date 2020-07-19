{ pkgs, ... }:

{
  programs = {
    bash = (import ./bash) pkgs;
    gnupg.agent.enable = true;
    less = (import ./less) pkgs;
    light.enable = true;
    ssh  = (import ./ssh);
    tmux = (import ./tmux) pkgs;
  };
}
