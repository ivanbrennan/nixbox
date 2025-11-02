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

  services.pipewire.enable = false;
  services.pulseaudio.enable = true;
  # NOTE: I added the following line while trying to get bluetooth headphones to
  # work well. I'm not entirely sure it's necessary, but will have to test the
  # behavior that results from removing/changing it.
  services.pulseaudio.package = pkgs.pulseaudioFull; # https://nixos.wiki/wiki/Bluetooth#Enabling_extra_codecs

  hardware.keyboard.qmk.enable = true;

  fonts = {
    packages = with pkgs; [
      cantarell-fonts
      dejavu_fonts
      emacs-all-the-icons-fonts
      fira-code
      mononoki

      # TODO: Now that nerdfonts has been split out into separate packages under
      # the namespace `nerd-fonts`, figure out which of those fonts I actually
      # want, and remove the others.
      nerd-fonts._0xproto
      nerd-fonts._3270
      nerd-fonts.adwaita-mono
      nerd-fonts.agave
      nerd-fonts.anonymice
      nerd-fonts.arimo
      nerd-fonts.atkynson-mono
      nerd-fonts.aurulent-sans-mono
      nerd-fonts.bigblue-terminal
      nerd-fonts.bitstream-vera-sans-mono
      nerd-fonts.blex-mono
      nerd-fonts.caskaydia-cove
      nerd-fonts.caskaydia-mono
      nerd-fonts.code-new-roman
      nerd-fonts.comic-shanns-mono
      nerd-fonts.commit-mono
      nerd-fonts.cousine
      nerd-fonts.d2coding
      nerd-fonts.daddy-time-mono
      nerd-fonts.dejavu-sans-mono
      nerd-fonts.departure-mono
      nerd-fonts.droid-sans-mono
      nerd-fonts.envy-code-r
      nerd-fonts.fantasque-sans-mono
      nerd-fonts.fira-code
      nerd-fonts.fira-mono
      nerd-fonts.geist-mono
      nerd-fonts.go-mono
      nerd-fonts.gohufont
      nerd-fonts.hack
      nerd-fonts.hasklug
      nerd-fonts.heavy-data
      nerd-fonts.hurmit
      nerd-fonts.im-writing
      nerd-fonts.inconsolata
      nerd-fonts.inconsolata-go
      nerd-fonts.inconsolata-lgc
      nerd-fonts.intone-mono
      nerd-fonts.iosevka
      nerd-fonts.iosevka-term
      nerd-fonts.iosevka-term-slab
      nerd-fonts.jetbrains-mono
      nerd-fonts.lekton
      nerd-fonts.liberation
      nerd-fonts.lilex
      nerd-fonts.martian-mono
      nerd-fonts.meslo-lg
      nerd-fonts.monaspace
      nerd-fonts.monofur
      nerd-fonts.monoid
      nerd-fonts.mononoki
      nerd-fonts."m+"
      nerd-fonts.noto
      nerd-fonts.open-dyslexic
      nerd-fonts.overpass
      nerd-fonts.profont
      nerd-fonts.proggy-clean-tt
      nerd-fonts.recursive-mono
      nerd-fonts.roboto-mono
      nerd-fonts.shure-tech-mono
      nerd-fonts.sauce-code-pro
      nerd-fonts.space-mono
      nerd-fonts.symbols-only
      nerd-fonts.terminess-ttf
      nerd-fonts.tinos
      nerd-fonts.ubuntu
      nerd-fonts.ubuntu-mono
      nerd-fonts.ubuntu-sans
      nerd-fonts.victor-mono
      nerd-fonts.zed-mono

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
