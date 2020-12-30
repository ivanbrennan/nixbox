pkgs:

{
  # Enable the X11 windowing system.
  enable = true;

  # Configure keyboard.
  layout = "us";
  xkbOptions = "ctrl:nocaps, shift:both_capslock";
  autoRepeatDelay = 200;
  autoRepeatInterval = 30;

  # Configure touchpad.
  libinput = {
    enable = true;
    naturalScrolling = true;
  };

  # desktopManager
  desktopManager.xterm.enable = false;

  # windowManager
  windowManager = {
    xmonad = {
      enable = true;
      extraPackages = ps: [ ps.xmonad-contrib ];
      config = ./xmonad.hs;
    };
  };

  displayManager.defaultSession = "none+xmonad";

  displayManager.lightdm.background = ''
    ${pkgs.nice-backgrounds}/share/backgrounds/gnome/moscow-subway.jpg
  '';
    # ${pkgs.nice-backgrounds}/share/backgrounds/gnome/owl-eye.jpg
    # ${pkgs.nice-backgrounds}/share/backgrounds/gnome/lookup.jpg

  displayManager.sessionCommands = ''
    ${pkgs.xorg.xrdb}/bin/xrdb -merge < ${./Xresources}
    ${pkgs.hsetroot}/bin/hsetroot -solid "#112026"
    ${pkgs.lxqt.lxqt-policykit}/bin/lxqt-policykit-agent &

    eval $(${pkgs.gnome3.gnome-keyring}/bin/gnome-keyring-daemon --daemonize)
    export SSH_AUTH_SOCK
  '';

  updateDbusEnvironment = true;
}
