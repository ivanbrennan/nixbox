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

  # Gnome desktop
  desktopManager.gnome3.enable = true;

  # i3
  windowManager.i3 = {
    enable = true;
    configFile = i3/config;
    extraSessionCommands = ''
      eval $(gnome-keyring-daemon --daemonize)
      export SSH_AUTH_SOCK
    '';
  };

  displayManager.lightdm.background = "#112026";

  displayManager.sessionCommands = ''
    ${pkgs.xorg.xrdb}/bin/xrdb -merge < ${./Xresources}

    # A cleaner solution will be available soon via:
    # services.xserver.windowManager.i3.extraSessionCommands
    # https://github.com/NixOS/nixpkgs/pull/49492#issuecomment-579311286
    if ! [ -e $HOME/.background-image ]
    then
        cp ${pkgs.nice-backgrounds}/share/backgrounds/gnome/solid-112026.png \
        $HOME/.background-image
    fi
  '';
}
