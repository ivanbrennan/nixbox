{ pkgs, ... }:

{
  imports = [
    "${builtins.fetchTarball { url = "https://github.com/NixOS/nixos-hardware/archive/d7a12fcc071bff59bd0ead589c975d802952a064.tar.gz"; sha256 = "1a213sa4smqwwhkwjsm2ccrzbq7mb0qrrw54jc2ik7q0v4x93ypn"; }}/lenovo/thinkpad/x1/9th-gen"
    ./hardware-configuration.nix
  ];

  networking = {
    hostName = "thinkpad9";

    interfaces.enp0s13f0u3.useDHCP = true;
    interfaces.wlp0s20f3.useDHCP = true;
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices = {
    root = {
      device = "/dev/disk/by-uuid/61a7b394-322f-4a1e-9b61-fff4cbf9a208";
      preLVM = true;
    };
  };

  virtualisation.libvirtd.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?
}
