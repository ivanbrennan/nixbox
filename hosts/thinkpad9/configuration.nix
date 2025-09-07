{ pkgs, nixos-hardware, ... }:

{
  imports = [
    ./hardware-configuration.nix
    nixos-hardware.nixosModules.lenovo-thinkpad-x1-9th-gen
  ];

  networking.hostName = "thinkpad9";
  networking.extraHosts = ''
    127.0.0.1 local.odeko.com
  '';
  security.pki.certificateFiles = [ ./localCA.pem ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices = {
    root = {
      device = "/dev/disk/by-partlabel/primary";
      bypassWorkqueues = true;
      preLVM = true;
    };
  };

  virtualisation.libvirtd.enable = true;

  hardware.bluetooth.enable = true;
  services.udev.extraRules = ''
    # TrackPoint Keyboard II (Bluetooth)
    ACTION=="add", SUBSYSTEM=="hid", ENV{HID_ID}=="0005:000017EF:000060E1", ATTR{fn_lock}="0"

    # TrackPoint Keyboard II (USB wireless)
    ACTION=="add", SUBSYSTEM=="hid", ENV{HID_ID}=="0003:000017EF:000060EE", ATTR{fn_lock}="0"
    ACTION=="add", SUBSYSTEM=="hid", ENV{HID_ID}=="0003:000017EF:000060EE", RUN+="${pkgs.coreutils}/bin/chgrp input %S%p/fn_lock"
    ACTION=="add", SUBSYSTEM=="hid", ENV{HID_ID}=="0003:000017EF:000060EE", RUN+="${pkgs.coreutils}/bin/chmod g+w %S%p/fn_lock"
  '';

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?
}
