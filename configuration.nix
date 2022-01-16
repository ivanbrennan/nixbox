# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Symlink: sudo make -C /etc/nixos machine=MACHINE
      "${builtins.fetchTarball { url = "https://github.com/NixOS/nixos-hardware/archive/d7a12fcc071bff59bd0ead589c975d802952a064.tar.gz"; sha256 = "1a213sa4smqwwhkwjsm2ccrzbq7mb0qrrw54jc2ik7q0v4x93ypn"; }}/lenovo/thinkpad/x1/9th-gen"
      ./hardware-configuration.nix

      # shared
      ./environment
    ];

  # Allow redshift to work.
  location.latitude = 40.7;
  location.longitude = -74.0;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices = {
    root = {
      device = "/dev/disk/by-uuid/61a7b394-322f-4a1e-9b61-fff4cbf9a208";
      preLVM = true;
    };
  };

  networking.hostName = "thinkpad"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/New_York";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  # networking.interfaces.enp0s13f0u2.useDHCP = true;
  networking.interfaces.wlp0s20f3.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  # Enable the X11 windowing system.
  # services.xserver.enable = true;

  nixpkgs.config = {
    allowUnfree = true;

    vim = {
      # Don't patch minimal nix support into vim. I'll use a plugin.
      ftNix = false;

      # Avoid cursor redraw bugs
      gui = "no";
    };
  };

  nixpkgs.overlays =
    [ (import ./overlays/core)
      (import ./overlays/haskell)
      (import ./overlays/odeko)
      (import ./overlays/vim)
    ];

  nix = {
    nixPath =
      [ "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos/nixpkgs"
        "nixpkgs-overlays=/etc/nixos/overlays"
        "nixos-config=/etc/nixos/configuration.nix"
        "/nix/var/nix/profiles/per-user/root/channels"
      ];

    envVars = {
      NIX_GITHUB_PRIVATE_USERNAME = import ./environment/github-username.private;
      NIX_GITHUB_PRIVATE_PASSWORD = import ./environment/github-token.private;
    };

    trustedUsers = [ "ivan" ];

    gc.automatic = true;
    gc.dates = "03:15";
  };

  fonts = {
    fonts = [
      pkgs.cantarell-fonts
      pkgs.dejavu_fonts
      pkgs.emacs-all-the-icons-fonts
      pkgs.mononoki
      pkgs.open-sans
      pkgs.source-code-pro
      pkgs.source-sans-pro
    ];

    fontconfig = {
      # The fontconfig service’s dpi option has been removed. Fontconfig should use Xft settings by default so there’s no need to override one value in multiple places. The user can set DPI via ~/.Xresources properly, or at the system level per monitor, or as a last resort at the system level with services.xserver.dpi.
      # dpi = 96;
      subpixel.rgba = "rgb";
    };
  };

  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

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
    hashedPassword = "$6$aAJGKQbaRR74cAFn$on/SNMV5pcVPH.dPaYQQgp1ZhBp3FA.0BoRRDQYndY.R8TdjMFqOmWP7kVTXZnpienIw7R/x8hmDeins/U.c/1";
    openssh.authorizedKeys.keys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDABjGqcHsTTDjmT30YUZ9VUJMz0cNYFqIRROz/7NVmS79gvIeS4/ll+flOtdgVcsDijjghHqA9AM/4OCv5sKICaufRV73PS4HKk06yfiCS2au5YzIg/jd+7gK5smxpS+55qtR0Yu1hOBrBik0Q2J7biLNpXLqHLnnrrrS5mkgnIRAb7Ojv/CQKT+ZDcusJWsZ7pzxY1BHqC59VNuy79knVbPAE44n6jnIXlfcIACVqmHlU/W6KVvxfkv+lncf2t6SAj3AuWdFD98YuWxN5QlGBPe+If5WwneYUc3ENjiSAJu1sHUYU9BMhe9YEFiCZVzKsv45Lr+1HlA225u447835 ivan.brennan@gmail.com" ];
  };
  users.mutableUsers = false;

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

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?

}

