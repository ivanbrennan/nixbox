# For help, see ‘man configuration.nix’ and ‘nixos-help’.

{ config, pkgs, ... }:

{
  imports =
    [ # Symlink: sudo make -C /etc/nixos machine=MACHINE
      ./machines/self.nix

      # shared
      ./cachix.nix
      ./environment
      ./programs
      ./security
      ./services
      ./users
      ./kmonad.nix

      # sops-nix
      (let
         rev = "c36df4fe4bf4bb87759b1891cab21e7a05219500";
         tar = builtins.fetchTarball {
           url = "https://github.com/Mic92/sops-nix/archive/${rev}.tar.gz";
           sha256 = "1hgd3a4yh7xs2ij9c41vga7chym7qdx0pgksrjka2smsdbpwncn9";
         };
       in "${tar}/modules/sops"
      )
    ];

  sops = {
    defaultSopsFile = ./sops-nix/secrets/secrets.yaml;
    age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
    secrets.example_key = {};
  };

  i18n.defaultLocale = "en_US.UTF-8";
  console.useXkbConfig = true;

  # Allow timezone to be set imperatively using:
  # timedatectl set-timezone America/New_York
  time.timeZone = null;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Allow redshift to work.
  location.latitude = 41.7;
  location.longitude = -74.1;

  virtualisation = {
    docker.enable = true;

    virtualbox.host = {
      enable = true;
      headless = true;
    };
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
      (import ./overlays/vim)
      (import ./overlays/emacs)
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

    settings.trusted-users = [ "ivan" ];

    gc.automatic = true;
    gc.dates = "03:15";

    extraOptions = ''
      experimental-features = nix-command flakes
    '';
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
  };

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  fonts = {
    fonts = with pkgs; [
      cantarell-fonts
      dejavu_fonts
      emacs-all-the-icons-fonts
      fira-code
      mononoki
      nerdfonts
      noto-fonts
      open-sans
      roboto-mono
      source-code-pro
      source-sans
      source-sans-pro
      source-serif
      vollkorn
    ];

    fontconfig = {
      subpixel.rgba = "rgb";
    };
  };
}
