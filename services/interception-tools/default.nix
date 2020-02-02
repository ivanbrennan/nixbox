pkgs:

{
  enable = true;

  plugins = with pkgs.interception-tools-plugins; [
    caps2esc
    space2meta
  ];

  udevmonConfig = ''
    - JOB: "intercept -g $DEVNODE | caps2esc | space2meta | uinput -d $DEVNODE"
      DEVICE:
        EVENTS:
          EV_KEY: [KEY_CAPSLOCK, KEY_ESC, KEY_SPACE]
  '';
}
