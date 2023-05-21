# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices = {
    root = {
      device = "/dev/disk/by-uuid/ef0fbbb1-f41f-4ae3-9139-1a81b12eb06a";
      preLVM = true;
    };
  };

  networking.hostName = "thinkpad6"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;  # Easiest to use and most distros use this by default.

  # Set your time zone.
  # time.timeZone = "America/New_York";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkbOptions in tty.
  # };

  # Enable the X11 windowing system.
  # services.xserver.enable = true;


  

  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = {
  #   "eurosign:e";
  #   "caps:escape" # map caps to escape.
  # };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.ivan = {
    isNormalUser = true;
    uid = 1000;
    createHome = true;
    home = "/home/ivan";
    extraGroups = [
      "wheel"
      "networkmanager"
    ];
    hashedPassword = "$y$j9T$vowJiSYMEds55yVoU3Rwn0$tdRX1zSuZRd8I.NJJwzYVEGKQn.ATGPXmH/ZubzmbE4";
    openssh.authorizedKeys.keys = [
      ''
        from="192.168.0.0/24" ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDABjGqcHsTTDjmT30YUZ9VUJMz0cNYFqIRROz/7NVmS79gvIeS4/ll+flOtdgVcsDijjghHqA9AM/4OCv5sKICaufRV73PS4HKk06yfiCS2au5YzIg/jd+7gK5smxpS+55qtR0Yu1hOBrBik0Q2J7biLNpXLqHLnnrrrS5mkgnIRAb7Ojv/CQKT+ZDcusJWsZ7pzxY1BHqC59VNuy79knVbPAE44n6jnIXlfcIACVqmHlU/W6KVvxfkv+lncf2t6SAj3AuWdFD98YuWxN5QlGBPe+If5WwneYUc3ENjiSAJu1sHUYU9BMhe9YEFiCZVzKsv45Lr+1HlA225u447835 ivan.brennan@gmail.com
      ''
    ];
  };
  users.mutableUsers = false;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    git
    gnupg
    pass
    vim
    wget
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.passwordAuthentication = false;
  services.openssh.permitRootLogin = "no";
  services.openssh.kbdInteractiveAuthentication = false;
  services.openssh.extraConfig = ''
    AllowAgentForwarding no
    StreamLocalBindUnlink no

    Match User ivan
      AllowAgentForwarding yes
      StreamLocalBindUnlink yes
  '';

  services.udisks2.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?

}

