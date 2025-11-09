pkgs:

{
  # Enable the X11 windowing system.
  enable = true;

  # Virtual console for the X server
  # tty = 7;

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

  displayManager.lightdm.background = ''
    ${pkgs.nice-backgrounds}/share/backgrounds/gnome/nix-wallpaper-binary-black_8k.png
  '';

  displayManager.sessionCommands = ''
    ${pkgs.xorg.xrdb}/bin/xrdb -merge ${./Xresources}
    ${pkgs.hsetroot}/bin/hsetroot -solid "#112026"
    if ! [ -e $HOME/.background-image ]
    then
        cp ${pkgs.nice-backgrounds}/share/backgrounds/gnome/nix-wallpaper-binary-black_8k.png \
        $HOME/.background-image
    fi
    ${pkgs.lxqt.lxqt-policykit}/bin/lxqt-policykit-agent &

    # Connect to the gnome-keyring-daemon that was started by PAM on login.
    eval $(${pkgs.gnome-keyring}/bin/gnome-keyring-daemon --start --components=secrets)

    # Hack: https://bugzilla.redhat.com/show_bug.cgi?id=2250704 still
    # applies to sessions not managed by systemd.
    if [ -z "$SSH_AUTH_SOCK" ] && [ -n "$XDG_RUNTIME_DIR" ]
    then
      export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/gcr/ssh"
    fi
  '';

  updateDbusEnvironment = true;

  videoDrivers = [ "modesetting" ];
}
