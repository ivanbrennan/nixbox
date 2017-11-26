# For help, see ‘man configuration.nix’ and ‘nixos-help’.

{ config, pkgs, ... }:

{
  imports =
    [
      ./bash-aliases.nix
      ./hardware-configuration.nix
      ./interactive-shell-init.nix
      ./prompt-init.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/disk/by-uuid/338a237d-0db7-4520-8ff5-3188533c59f6";
      preLVM = true;
    }
  ];

  networking.hostName = "nixosbox";
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Select internationalisation properties.
  i18n = {
  #   consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "America/New_York";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    chromium
    git
    mkpasswd
    slack
    tmux
    tree
    vimHugeX
    xcape
  ];

  nixpkgs.config.allowUnfree = true;

  environment.variables = {
    DOTFILES = "$HOME/Development/resources/dotfiles-nixos";
    EDITOR = "vim";
    VISUAL = "vim";
    GIT_EDITOR = "vim";
    GIT_MERGE_AUTOEDIT = "no";
  };

  environment.etc."inputrc".source = pkgs.lib.mkForce ./inputrc;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.bash.enableCompletion = true;
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  services.xserver = import ./services/xserver.nix;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.ivan = {
    isNormalUser = true;
    uid = 1000;
    createHome = true;
    home = "/home/ivan";
    extraGroups = [
      "wheel"
      "networkmanager"
    ];
    hashedPassword = "$6$4uOYQEuFA$RqNmGNfQcR6mPK2.jSHPntF43HgN6BJP4nwQANNUbp8ulpquniQNqecgUMVGRBsjBzt2b7gJBhCCedbUYmI/60";
  };
  users.mutableUsers = false;

  nix.gc.automatic = true;
  nix.gc.dates = "03:15";

  system.autoUpgrade.enable = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "17.09"; # Did you read the comment?

}
