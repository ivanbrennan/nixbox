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
  libinput.enable = true;
  libinput.naturalScrolling = true;

  # Gnome desktop
  desktopManager.gnome3.enable = true;

  # i3
  windowManager.i3.enable = true;
  windowManager.i3.configFile = i3/config;

  displayManager.sessionCommands = ''
    ${pkgs.xorg.xrdb}/bin/xrdb -merge < ${./Xresources}
  '';
}
