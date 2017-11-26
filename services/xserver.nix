{
  # Enable the X11 windowing system.
  enable = true;
  layout = "us";
  xkbOptions = "caps:ctrl_modifier, shift:both_capslock";

  # Enable touchpad support.
  libinput.enable = true;

  displayManager.sessionCommands = ''
    # tap caps to escape
    xcape -e 'Caps_Lock=Escape' &
  '';

  # Gnome desktop
  desktopManager = {
    gnome3.enable = true;
    default = "gnome3";
  };
}
