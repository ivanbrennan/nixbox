{
  imports = [ ./hardware-configuration.nix ];

  networking.hostName = "littleThinkPad";

  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/sda";
  };

  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/sda2";
      preLVM = true;
    }
  ];
}
