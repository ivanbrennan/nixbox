# For help, see ‘man configuration.nix’ and ‘nixos-help’.

{ config, pkgs, ... }:

{
  imports =
    [ # Symlink: sudo make -C /etc/nixos machine=MACHINE
      ./machines/self.nix

      # shared
      ./environment
      ./programs
      ./services
      ./users
    ];

  i18n = {
    defaultLocale = "en_US.UTF-8";
    consoleUseXkbConfig = true;
  };

  time.timeZone = "America/New_York";

  virtualisation.docker.enable = true;

  nixpkgs.config = {
    allowUnfree = true;

    vim = {
      # Don't patch minimal nix support into vim. I'll use a plugin.
      ftNix = false;

      # Avoid cursor redraw bugs
      gui = "no";
    };
  };

  nixpkgs.overlays =
    [ (import ./overlays/core)
      (import ./overlays/vim)
    ];

  nix = {
    nixPath =
      [ "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos/nixpkgs"
        "nixpkgs-overlays=/etc/nixos/overlays"
        "nixos-config=/etc/nixos/configuration.nix"
        "/nix/var/nix/profiles/per-user/root/channels"
      ];
    gc.automatic = true;
    gc.dates = "03:15";
  };

  # see machines/self.nix for hostName
  networking = {
    nameservers =
      [ # cloudflare IPv4
        "1.1.1.1"
        "1.0.0.1"
        # cloudflare IPv6
        "2606:4700:4700::1111"
        "2606:4700:4700::1001"
      ];

    firewall.allowedTCPPorts = [ ];
  };

  fonts.fonts = [
    pkgs.source-code-pro
    pkgs.emacs-all-the-icons-fonts
  ];

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "17.09"; # Did you read the comment?

}
