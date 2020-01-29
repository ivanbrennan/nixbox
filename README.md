## make

``` sh
sudo make -C /etc/nixos machine=MACHINE
```

## Todo
Try some window managers
- [i3](https://i3wm.org)
  - set $mod to super
  - run dmenu (or j4) by pressing $mod alone
  - desktop.interface gtk-key-theme "Emacs"
  - desktop.interface clock-format "12h"
  - desktop.peripherals.touchpad tap-to-click true
  - desktop.peripherals.touchpad click-method "fingers"
  - desktop.peripherals.keyboard delay 200
  - desktop.peripherals.keyboard repeat-interval 30
  - Terminal.Legacy.Settings default-show-menubar false
  - Terminal.Legacy.Settings theme-variant "dark"
  - gnome_terminal_set scrollbar-policy "never"
  - keybindings=Keybindings:/org/gnome/terminal/legacy/keybindings/
  - Terminal.Legacy.${keybindings} copy '<Ctrl><Alt>c'
  - Terminal.Legacy.${keybindings} paste '<Ctrl><Alt>v'
  - Terminal.Legacy.${keybindings} new-window '<Ctrl><Alt>n'
  - Terminal.Legacy.${keybindings} new-tab '<Ctrl><Alt>t'
  - Terminal.Legacy.${keybindings} toggle-menubar '<Ctrl><Alt>b'
  - Terminal.Legacy.${keybindings} full-screen '<Ctrl><Alt>Return'
  - Terminal.Legacy.${keybindings} zoom-in '<Ctrl>equal'
  - desktop.wm.keybindings activate-window-menu "['<Alt><Shift>space']"
  - desktop.background picture-uri '/run/current-system/sw/share/backgrounds/gnome/Godafoss_Iceland.jpg'
  - GPaste launch-ui '<Ctrl><Alt>G'
  - GPaste pop '<Ctrl><Alt>P'
  - GPaste show-history '<Ctrl><Alt>H'
  - GPaste sync-clipboard-to-primary '<Ctrl><Alt><Shift>O'
  - GPaste sync-primary-to-clipboard '<Ctrl><Alt><Shift>P'
  - GPaste make-password '<Ctrl><Alt>S'
  - GPaste upload '<Ctrl><Alt><Shift>U'
  - easily logout / lock screen?
  - configure urxvt and/or alacritty
  - configure i3 hjkl
  - xcape stopped working?
  - hide cursor
- [Openbox](http://openbox.org/wiki/Main_Page)
- [XMonad](http://xmonad.org)
- [awesome WM](https://awesomewm.org)
- [dwm](https://dwm.suckless.org)
