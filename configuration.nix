# For help, see ‘man configuration.nix’ and ‘nixos-help’.

{ config, pkgs, ... }:

{
  imports =
    [ # Symlink: sudo make -C /etc/nixos machine=MACHINE
      ./machines/self.nix

      # shared
      ./environment
      ./programs
      ./security
      ./services
      # ./users # TODO: upgrade nixosbox to 21.11
      ./kmonad.nix
    ];

  i18n.defaultLocale = "en_US.UTF-8";
  console.useXkbConfig = true;

  # Allow timezone to be set imperatively using:
  # timedatectl set-timezone America/New_York
  time.timeZone = null;

  # Allow redshift to work.
  location.latitude = 40.7;
  location.longitude = -74.0;

  virtualisation = {
    docker.enable = true;

    # Checking that Nix store paths of all wrapped programs exist... FAIL
    # The path /nix/store/lhxpydkbbzxc59hp7my9zvd75andh4rc-virtualbox-6.1.30/libexec/virtualbox/VBoxSDL does not exist!
    # Please, check the value of `security.wrappers."VBoxSDL".source`.
    # virtualbox.host = {
    #   enable = true;
    #   headless = true;
    # };
  };

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

  # see machines/self.nix for hostName
  networking = {
    nameservers =
      [ # cloudflare IPv4
        "1.1.1.1"
        "1.0.0.1"
        # cloudflare IPv6
        "2606:4700:4700::1111"
        "2606:4700:4700::1001"
      ];

    firewall.allowedTCPPorts = import ./allowed-tcp-ports.nix.private;
    firewall.allowedUDPPorts = [ ];

    networkmanager.enable = true;

    # The global useDHCP flag is deprecated, therefore explicitly set to false
    # here. Per-interface useDHCP will be mandatory in the future, so this
    # generated config replicates the default behaviour.
    useDHCP = false;
    # interfaces.enp0s13f0u2.useDHCP = true;
    interfaces.wlp0s20f3.useDHCP = true;
  };

  sound.enable = true;
  hardware.pulseaudio.enable = true;

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

  users.users.ivan = {
    isNormalUser = true;
    uid = 1000;
    createHome = true;
    home = "/home/ivan";
    extraGroups = [
      "dialout"
      "docker"
      "input"
      "networkmanager"
      "uinput"
      "vboxusers"
      "video"
      "wheel"
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
}
