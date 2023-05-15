{ pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  networking.hostName = "nixosbox";

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices = {
    root = {
      device = "/dev/disk/by-uuid/01f0c2eb-300c-49e9-9b5f-0180f45445b0";
      preLVM = true;
    };
  };

  boot.kernel.sysctl."vm.swappiness" = 1;

  hardware.cpu.intel.updateMicrocode = true;

  hardware.opengl.extraPackages = with pkgs; [
    vaapiIntel
    vaapiVdpau
    libvdpau-va-gl
  ];

  services.xserver.videoDrivers = [ "intel" ];
  services.kmonad = {
    enable = true;
    configfile = ./config.kbd;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "17.09"; # Did you read the comment?
}
