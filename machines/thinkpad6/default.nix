{ pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      # ( let
      #     nixos-hardware = builtins.fetchTarball {
      #       url = "https://github.com/NixOS/nixos-hardware/archive/71ce85372a614d418d5e303dd5702a79d1545c04.tar.gz";
      #       sha256 = "0000000000000000000000000000000000000000000000000000";
      #     };
      #   in "${nixos-hardware}/lenovo/thinkpad/x1/6th-gen"
      # )
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices = {
    root = {
      device = "/dev/disk/by-partlabel/primary";
      preLVM = true;
    };
  };

  networking.hostName = "thinkpad6"; # Define your hostname.

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?

}

