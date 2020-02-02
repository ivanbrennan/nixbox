{ pkgs, ... }:

{
  programs = {
    bash = (import ./bash) pkgs;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
    less = (import ./less) pkgs;
    ssh  = (import ./ssh);
    tmux = (import ./tmux) pkgs;
  };
}
