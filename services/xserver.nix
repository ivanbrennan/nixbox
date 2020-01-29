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

  # displayManager is set to lightdm
  # the "none+i3" entry denotes "none" desktop manager + "i3" window manager
}
