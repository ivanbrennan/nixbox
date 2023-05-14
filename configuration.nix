# For help, see ‘man configuration.nix’ and ‘nixos-help’.

{ config, pkgs, ... }:

{
  imports =
    [ # Symlink: sudo make -C /etc/nixos machine=MACHINE
      ./machines/self.nix

      # shared
      ./cachix.nix
      ./environment
      ./programs
      ./security
      ./services
      ./users
      ./kmonad.nix
    ];

  i18n.defaultLocale = "en_US.UTF-8";
  console.useXkbConfig = true;

  # Allow timezone to be set imperatively using:
  # timedatectl set-timezone America/New_York
  time.timeZone = null;

  # Allow redshift to work.
  location.latitude = 41.7;
  location.longitude = -74.1;

  virtualisation = {
    docker.enable = true;

    virtualbox.host = {
      enable = true;
      headless = true;
    };
  };

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
      (import ./overlays/haskell)
      (import ./overlays/vim)
    ];

  nix = {
    nixPath =
      [ "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos/nixpkgs"
        "nixpkgs-overlays=/etc/nixos/overlays"
        "nixos-config=/etc/nixos/configuration.nix"
        "/nix/var/nix/profiles/per-user/root/channels"
      ];

    envVars = {
      NIX_GITHUB_PRIVATE_USERNAME = import ./environment/github-username.private;
      NIX_GITHUB_PRIVATE_PASSWORD = import ./environment/github-token.private;
    };

    settings.trusted-users = [ "ivan" ];

    gc.automatic = true;
    gc.dates = "03:15";

    extraOptions = ''
      experimental-features = nix-command flakes
    '';
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

    firewall.allowedTCPPorts = import ./allowed-tcp-ports.nix.private;
    firewall.allowedUDPPorts = [ ];

    networkmanager.enable = true;
  };

  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull; # support Bluetooth headsets
  };
  hardware.bluetooth.enable = true;

  fonts = {
    fonts = [
      pkgs.cantarell-fonts
      pkgs.dejavu_fonts
      pkgs.emacs-all-the-icons-fonts
      pkgs.fira-code
      pkgs.mononoki
      pkgs.nerdfonts
      pkgs.noto-fonts
      pkgs.open-sans
      pkgs.roboto-mono
      pkgs.source-code-pro
      pkgs.source-sans
      pkgs.source-serif
      pkgs.vollkorn
    ];

    fontconfig = {
      subpixel.rgba = "rgb";
    };
  };
}
