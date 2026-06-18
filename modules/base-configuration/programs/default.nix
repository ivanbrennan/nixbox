{ pkgs, ... }:

{
  programs = {
    bash = (import ./bash) pkgs;
    dconf.enable = true;
    chromium = (import ./chromium) pkgs;
    gnupg.agent = {
      enable = true;
      settings = {
        default-cache-ttl = 28800;
        max-cache-ttl = 86400;
      };
    };
    hyprland.enable = true;
    i3lock.enable = true;
    less = (import ./less) pkgs;
    neovim = (import ./neovim) pkgs;
    nix-index.enable = true;
    nm-applet.enable = true;
    seahorse.enable = true;
    ssh  = (import ./ssh);
    thunar = {
      enable = true;
      plugins = with pkgs; [ thunar-volman tumbler ];
    };
    tmux = (import ./tmux) pkgs;
    wireshark = {
      enable = true;
      package = pkgs.wireshark;
    };
  };
}
