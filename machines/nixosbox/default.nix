{
  imports = [ ./hardware-configuration.nix ];

  networking.hostName = "nixosbox";
  networking.firewall.allowedTCPPorts = [ ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/disk/by-uuid/01f0c2eb-300c-49e9-9b5f-0180f45445b0";
      preLVM = true;
    }
  ];
}
