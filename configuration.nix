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

  nixpkgs.overlays = (import ./overlays);

  # see machines/self.nix for hostName
  networking = {
    nameservers = [ "1.1.1.1" ];

    firewall.allowedTCPPorts = [ ];
  };

  fonts.fonts = [
    pkgs.source-code-pro
    pkgs.emacs-all-the-icons-fonts
  ];

  nix.gc.automatic = true;
  nix.gc.dates = "03:15";

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "17.09"; # Did you read the comment?

}
