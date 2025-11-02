{ config, pkgs, ... }:

{
  services = {
    displayManager.defaultSession = "none+xmonad";

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

    xserver = (import ./xserver) pkgs;

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
