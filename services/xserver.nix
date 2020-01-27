{
  # Enable the X11 windowing system.
  enable = true;
  layout = "us";
  xkbOptions = "ctrl:nocaps, shift:both_capslock";

  # Enable touchpad support.
  libinput.enable = true;

  # Gnome desktop
  desktopManager.gnome3.enable = true;
}
