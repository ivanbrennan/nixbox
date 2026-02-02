{ pkgs, lib }:

{
  # Enable the X11 windowing system.
  enable = true;

  # Allow startx-minimal to determine appropriate display dynamically based on
  # the virtual TTY from which it was invoked.
  display = null;

  # Configure keyboard.
  enableCtrlAltBackspace = true;
  xkb = {
    layout = "us";
    options = "ctrl:nocaps, shift:both_capslock, terminate:ctrl_alt_bksp";
  };
  autoRepeatDelay = 200;
  autoRepeatInterval = 30;

  # desktopManager
  desktopManager.xterm.enable = false;
  desktopManager.wallpaper.mode = "fill";

  # windowManager
  windowManager.xmonad = import ./xmonad;

  # Normally, enabling startx changes logFile to null so that no `-logfile` arg
  # will be passed to Xorg. That allows Xorg to log to its default log location
  # ($XDG_DATA_HOME/xorg/). I would rather it didn't, because I run startx via
  # systemd-cat, which sends logs to the journal.
  logFile = "/dev/null";

  displayManager.startx = {
    enable = true;
    generateScript = true; # /etc/static/X11/xinit/xinitrc
    extraCommands = ''
      ${pkgs.xorg.xrdb}/bin/xrdb -merge ${./Xresources}
      ${pkgs.xorg.xsetroot}/bin/xsetroot -cursor_name left_ptr
      if ! [ -e $HOME/.background-image ]
      then
          cp ${pkgs.nice-backgrounds}/share/backgrounds/gnome/nix-wallpaper-binary-black_8k.png \
          $HOME/.background-image
      fi
      ${pkgs.feh}/bin/feh --bg-fill $HOME/.background-image
      ${pkgs.lxqt.lxqt-policykit}/bin/lxqt-policykit-agent &

      # Connect to the gnome-keyring-daemon that was started by PAM on login.
      eval $(${pkgs.gnome-keyring}/bin/gnome-keyring-daemon --start --components=secrets)

      # Hack: https://bugzilla.redhat.com/show_bug.cgi?id=2250704 still
      # applies to sessions not managed by systemd.
      if [ -z "$SSH_AUTH_SOCK" ] && [ -n "$XDG_RUNTIME_DIR" ]
      then
        export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/gcr/ssh"
      fi

      export XDG_CURRENT_DESKTOP=XMonad
      export XDG_SESSION_DESKTOP=xmonad
      export XDG_SESSION_TYPE=x11
      ${lib.getBin pkgs.dbus}/bin/dbus-update-activation-environment --systemd --all
    '';
  };

  videoDrivers = [ "modesetting" ];
}
