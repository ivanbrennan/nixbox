# For help, see ‘man configuration.nix’ and ‘nixos-help’.

{ config, pkgs, ... }:

{
  imports =
    [ # Symlink to ./machines/HOSTNAME.nix
      # sudo ln -sr /etc/nixos/machines/{HOSTNAME/default,self}.nix
      ./machines/self.nix

      ./environment/default.nix
      ./programs/default.nix
      ./services/default.nix
      ./users/default.nix
    ];

  # Select internationalisation properties.
  i18n = {
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "America/New_York";

  virtualisation.docker.enable = true;

  nixpkgs.config.allowUnfree = true;

  nix.gc.automatic = true;
  nix.gc.dates = "03:15";

  system.autoUpgrade.enable = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "17.09"; # Did you read the comment?

}
