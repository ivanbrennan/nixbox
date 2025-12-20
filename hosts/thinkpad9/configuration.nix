{ config, pkgs, nixos-hardware, ... }:

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

  # NOTE: I added the following line while trying to get bluetooth headphones to
  # work well. I'm not entirely sure it's necessary, but will have to test the
  # behavior that results from removing/changing it.
  hardware.bluetooth.settings.General.Enable = "Source,Sink,Media,Socket"; # https://nixos.wiki/wiki/Bluetooth#Enabling_A2DP_Sink

  services.udev.extraRules = ''
    # TrackPoint Keyboard II (Bluetooth)
    ACTION=="add", SUBSYSTEM=="hid", ENV{HID_ID}=="0005:000017EF:000060E1", RUN+="${config.systemd.package}/bin/systemctl start fnlock-off.service"

    # TrackPoint Keyboard II (USB wireless)
    ACTION=="add", SUBSYSTEM=="hid", ENV{HID_ID}=="0003:000017EF:000060EE", ATTR{fn_lock}="0"
    ACTION=="add", SUBSYSTEM=="hid", ENV{HID_ID}=="0003:000017EF:000060EE", RUN+="${pkgs.coreutils}/bin/chgrp input %S%p/fn_lock"
    ACTION=="add", SUBSYSTEM=="hid", ENV{HID_ID}=="0003:000017EF:000060EE", RUN+="${pkgs.coreutils}/bin/chmod g+w %S%p/fn_lock"
  '';

  systemd.services.fnlock-off = {
    description = "Disable FnLock for TrackPoint Keyboard II (Bluetooth)";
    serviceConfig.Type = "oneshot";
    script = ''
      for _ in $(${pkgs.coreutils}/bin/seq 1 100)
      do
          ${pkgs.coreutils}/bin/sleep 0.02

          for fn_lock in /sys/bus/hid/devices/0005:17EF:60E1.*/fn_lock
          do
              if [ -w "$fn_lock" ]
              then
                  echo 0 > "$fn_lock"
                  exit 0
              fi
          done
      done
    '';
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?
}
