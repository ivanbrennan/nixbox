let
  xkbOptions = "ctrl:nocaps, shift:both_capslock";
in
{
  # Enable the X11 windowing system.
  enable = true;
  layout = "us";
  xkbOptions = xkbOptions;

  # Enable touchpad support.
  libinput.enable = true;

  displayManager.sessionCommands = ''
    gsettings set org.gnome.desktop.input-sources xkb-options "['${xkbOptions}']"

    # tap caps to escape
    xcape &
  '';

  # Gnome desktop
  desktopManager = {
    gnome3.enable = true;
    default = "gnome3";
  };
}
