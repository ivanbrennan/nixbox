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

  # windowManager
  windowManager.xmonad = import ./xmonad;

  displayManager.lightdm.background = ''
    ${pkgs.nice-backgrounds}/share/backgrounds/gnome/moscow-subway.jpg
  '';
    # ${pkgs.nice-backgrounds}/share/backgrounds/gnome/owl-eye.jpg
    # ${pkgs.nice-backgrounds}/share/backgrounds/gnome/lookup.jpg

  displayManager.sessionCommands = ''
    ${pkgs.xorg.xrdb}/bin/xrdb -merge ${./Xresources}
    ${pkgs.hsetroot}/bin/hsetroot -solid "#112026"
    ${pkgs.lxqt.lxqt-policykit}/bin/lxqt-policykit-agent &

    eval $(${pkgs.gnome-keyring}/bin/gnome-keyring-daemon --daemonize)
    export SSH_AUTH_SOCK
  '';

  updateDbusEnvironment = true;

  videoDrivers = [ "modesetting" ];
}
