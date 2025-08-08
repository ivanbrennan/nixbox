# For help, see ‘man configuration.nix’ and ‘nixos-help’.

{ config, pkgs, sops-nix, ... }:

{
  imports =
    [ sops-nix.nixosModules.sops
      ./environment
      ./programs
      ./security
      ./services
      ./users
      ./kmonad.nix
    ];

  sops = {
    defaultSopsFile = ./secrets.yaml;
    age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
    secrets = {
      # TODO: https://github.com/Mic92/sops-nix#restartingreloading-systemd-units-on-secret-change
      openvpn_pia_login = {};
      openvpn_pia_ca = {};
      openvpn_pia_crl = {};
      openvpn_odeko_ta = {};
      openvpn_odeko_ca = {};
      openvpn_odeko_cert = {};
      openvpn_odeko_key = {};
      openvpn_odeko_askpass = {};
      environment_etc_nix_netrc = {
        path = "/etc/nix/netrc";
      };
      docspell_joex_config = {
        owner = config.users.users.docspell.name;
        group = config.users.users.docspell.group;
        restartUnits = [ "docspell-joex.service" ];
      };
      docspell_restserver_config = {
        owner = config.users.users.docspell.name;
        group = config.users.users.docspell.group;
        restartUnits = [ "docspell-restserver.service" ];
      };
      localhost_ca_root_cert = {
        owner = config.users.users.docspell.name;
        group = config.users.users.docspell.group;
        restartUnits = [
          "docspell-joex.service"
          "docspell-restserver.service"
        ];
      };
      docspell_pginit = {
        owner = config.systemd.services.postgresql.serviceConfig.User;
        restartUnits = [ "postgresql.service" ];
      };
      postgresql_ssl_cert = {
        owner = config.systemd.services.postgresql.serviceConfig.User;
        group = config.systemd.services.postgresql.serviceConfig.User;
        restartUnits = [ "postgresql.service" ];
      };
      postgresql_ssl_key = {
        owner = config.systemd.services.postgresql.serviceConfig.User;
        group = config.systemd.services.postgresql.serviceConfig.User;
        mode = "0600";
        restartUnits = [ "postgresql.service" ];
      };
    };
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

  nix = {
    nixPath =
      [ "nixpkgs-overlays=/etc/nixos/overlays"
        "nixos-config=/etc/nixos/configuration.nix"
        "/nix/var/nix/profiles/per-user/root/channels"
      ];

    settings = {
      trusted-users = [ "ivan" ];
      experimental-features = [ "nix-command" "flakes" ];
      extra-substituters = [
        "https://nix-community.cachix.org"
      ];
      extra-trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };

    gc.automatic = true;
    gc.dates = "03:15";
  };

  networking = {
    nameservers =
      [ # cloudflare IPv4
        "1.1.1.1"
        "1.0.0.1"
        # cloudflare IPv6
        "2606:4700:4700::1111"
        "2606:4700:4700::1001"
      ];

    firewall.allowedTCPPorts = builtins.genList (x: 19090 + x) 10;
    firewall.allowedUDPPorts = [ ];

    networkmanager.enable = true;
  };

  # Temporary workaround
  # https://github.com/NixOS/nixpkgs/issues/296953
  systemd.services.NetworkManager-wait-online = {
    serviceConfig = {
      ExecStart = [ "" "${pkgs.networkmanager}/bin/nm-online -q" ];
    };
  };

  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.keyboard.qmk.enable = true;

  fonts = {
    packages = with pkgs; [
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
