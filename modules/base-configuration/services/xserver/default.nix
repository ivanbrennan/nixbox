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

    eval $(${pkgs.gnome-keyring}/bin/gnome-keyring-daemon --daemonize)
    export SSH_AUTH_SOCK
  '';

  updateDbusEnvironment = true;

  videoDrivers = [ "modesetting" ];
}
