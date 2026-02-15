{ config, pkgs, lib, ... }:

let
  desktop-sessions = (import ./desktop-sessions) { inherit pkgs lib; };

  greeters = {
    agreety = {
      useText = true;
      command = "${pkgs.greetd}/bin/agreety --cmd '${pkgs.start-xsession}/bin/start-xsession'";
    };
    tuigreet = {
      useText = true;
      command =
        let
          args = [
            "--remember"
            "--remember-session"
            "--sessions ${desktop-sessions}/share/wayland-sessions"
            "--xsessions ${desktop-sessions}/share/xsessions"
            "--no-xsession-wrapper"
          ];
        in
          "${lib.getExe pkgs.tuigreet} ${lib.concatStringsSep " " args}";
    };
    gtkgreet = {
      useText = false;
      command = "systemd-cat --identifier=cage-greeter -- ${lib.getExe pkgs.cage} -d -s -m last -- ${pkgs.gtkgreet}/bin/gtkgreet";
    };
  };

in
{
  environment.sessionVariables = {
    XDG_DATA_DIRS = lib.mkBefore [ "${desktop-sessions}/share" ];
  };

  xdg.portal.config = {
    xmonad = {
      default = "gtk";
      "org.freedesktop.impl.portal.Secret" = "gnome-keyring";
    };
  };

  # Enable a systemd-based initrd so the LUKS passphrase entered at boot
  # is stored in the kernel keyring.
  boot.initrd.systemd.enable = true;

  # Configure greetd PAM service to:
  #  - delegate most operations to the login PAM service
  #  - transfer the LUKS passphrase into the PAM session during auto-login
  #  - inject the passphrase for use by gnome-keyring
  security.pam.services.greetd.text = ''
    auth      substack      login
    account   include       login
    password  substack      login
    session   optional      ${pkgs.pam_fde_boot_pw}/lib/security/pam_fde_boot_pw.so inject_for=gkr
    session   include       login
  '';

  # Configure greetd-autologin PAM service to:
  #  - delegate most operations to the greetd PAM service
  #  - set XDG_SESSION_TYPE early, so that the correct session type is set when
  #    PAM registers the session in systemd-logind (via pam_systemd).
  security.pam.services.greetd-autologin.text =
    let
      conffile = pkgs.writeText "greetd-autologin-environment" ''
        XDG_SESSION_TYPE   DEFAULT="x11"
      '';
    in
    ''
      auth      include       greetd
      account   include       greetd
      password  include       greetd
      session   optional      pam_env.so conffile=${conffile} readenv=0
      session   include       greetd
    '';

  services = {
    greetd =
      let
        greeter = greeters.tuigreet;
      in
      {
        enable = true;
        useTextGreeter = greeter.useText;
        settings = {
          # Automatically start a user session on boot.
          initial_session = {
            user = config.users.users.ivan.name;
            command = "start-xsession";
            service = "greetd-autologin";
          };

          # Start a greeter after the initial session exits.
          default_session = {
            command = greeter.command;
          };
        };
      };

    emacs.enable = true;

    gvfs.enable = true;

    interception-tools = {
      enable = true;
      udevmonConfig = ''
        - CMD: "${pkgs.interception-tools}/bin/mux -c caps2esc"
        - JOB: "${pkgs.interception-tools}/bin/mux -i caps2esc | ${pkgs.interception-tools-plugins.caps2esc}/bin/caps2esc -m 1 | ${pkgs.interception-tools}/bin/uinput -c /etc/interception/keyboard.yaml"
        - JOB: "${pkgs.interception-tools}/bin/intercept -g $DEVNODE | ${pkgs.interception-tools}/bin/mux -o caps2esc"
          DEVICE:
            EVENTS:
              EV_KEY: [[KEY_CAPSLOCK, KEY_ESC, KEY_LEFTCTRL]]
            LINK: .*-event-kbd
        - JOB: "${pkgs.interception-tools}/bin/intercept -g $DEVNODE | ${pkgs.interception-tools}/bin/mux -o caps2esc"
          DEVICE:
            EVENTS:
              EV_KEY: [[KEY_CAPSLOCK, KEY_ESC, KEY_LEFTCTRL]]
            NAME: .*[kK]eychron .*[kK]eyboard.*
            BUSTYPE: 5
        - JOB: "${pkgs.interception-tools}/bin/intercept $DEVNODE | ${pkgs.interception-tools}/bin/mux -o caps2esc"
          DEVICE:
            EVENTS:
              EV_KEY: [BTN_LEFT, BTN_TOUCH]
      '';
    };

    libinput = {
      enable = true;
      touchpad.naturalScrolling = true;
    };

    journald.extraConfig = "SystemMaxUse=2G";

    locate.enable = true;
    locate.pruneNames = [];

    openvpn = (import ./openvpn) { inherit (config.sops) secrets; };

    redshift = {
      enable = true;
      temperature = {
        day = 5250;
        night = 3700;
      };
    };

    xbanish.enable = true;

    xserver = (import ./xserver) { inherit pkgs lib; };

    picom = {
      enable = true;
      fade = false;
      shadow = false;

      # glx (OpenGL) backend performs all rendering operations with OpenGL.
      # It is more friendly to some VSync methods, and has significantly
      # superior performance on color inversion (--invert-color-include) or
      # blur (--blur-background). You may wish to look at the GLX performance
      # optimization options below. --xrender-sync-fence might be needed on some
      # systems to avoid delay in changes of screen contents.
      backend = "glx";

      vSync = true;

      settings = {
        # --glx-no-stencil
        #     GLX backend: Avoid using stencil buffer, useful if you don’t have
        #     a stencil buffer. Might cause incorrect opacity when rendering
        #     transparent content (but never practically happened) and may not
        #     work with --blur-background. My tests show a 15% performance
        #     boost. Recommended.
        glx-no-stencil = true;

        # --glx-no-rebind-pixmap
        #     GLX backend: Avoid rebinding pixmap on window damage. Probably
        #     could improve performance on rapid window content changes, but is
        #     known to break things on some drivers (LLVMpipe, xf86-video-intel,
        #     etc.). Recommended if it works.
        # glx-no-rebind-pixmap = true;

        # --xrender-sync-fence
        #     Use X Sync fence to sync clients' draw calls, to make sure all
        #     draw calls are finished before picom starts drawing. Needed on
        #     nvidia-drivers with GLX backend for some users.
        # xrender-sync-fence = true;

        # --unredir-if-possible
        #     Unredirect all windows if a full-screen opaque window is detected,
        #     to maximize performance for full-screen windows. Known to cause
        #     flickering when redirecting/unredirecting windows.
        unredir-if-possible = true;
      };
    };

    colord.enable = true;

    # Enable GNOME keyring service and configure the login PAM stack to unlock
    # the user's default keyring when a password is available.
    gnome.gnome-keyring.enable = true;

    gnome.gcr-ssh-agent.enable = true;
    hardware.bolt.enable = true;
    udisks2.enable = true;
    upower.enable = config.powerManagement.enable;
    openssh = {
      enable = true;
      settings = {
        PermitRootLogin = "no";
        PasswordAuthentication = false;
        KbdInteractiveAuthentication = false;
      };
      extraConfig = ''
        AllowAgentForwarding no
        StreamLocalBindUnlink no

        Match User ivan
          AllowAgentForwarding yes
          StreamLocalBindUnlink yes
      '';
    };
  };
}
