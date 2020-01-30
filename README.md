## make

``` sh
sudo make -C /etc/nixos machine=MACHINE
```

## Todo
Try some window managers
- [i3](https://i3wm.org)
  - set $mod to super
  - run dmenu (or j4) by pressing $mod alone
  - equivalent gsettings
    - [✓] desktop.interface gtk-key-theme "Emacs"
    - [ ] desktop.interface clock-format "12h"
    - [✓] services.xserver.libinput.tapping
    - [✓] services.xserver.libinput.clickMethod
    - [✓] services.xserver.autoRepeatDelay
    - [✓] services.xserver.autoRepeatInterval
    - [ ] Terminal.Legacy.${keybindings} copy '<Ctrl><Alt>c'
    - [ ] Terminal.Legacy.${keybindings} paste '<Ctrl><Alt>v'
    - [ ] Terminal.Legacy.${keybindings} new-window '<Ctrl><Alt>n'
    - [ ] Terminal.Legacy.${keybindings} new-tab '<Ctrl><Alt>t'
    - [ ] Terminal.Legacy.${keybindings} toggle-menubar '<Ctrl><Alt>b'
    - [ ] Terminal.Legacy.${keybindings} full-screen '<Ctrl><Alt>Return'
    - [ ] Terminal.Legacy.${keybindings} zoom-in '<Ctrl>equal'
    - [ ] desktop.wm.keybindings activate-window-menu "['<Alt><Shift>space']"
    - [ ] desktop.background picture-uri '/run/current-system/sw/share/backgrounds/gnome/Godafoss_Iceland.jpg'
  - easily logout / lock screen?
  - configure urxvt and/or alacritty
  - configure i3 hjkl
  - xcape stopped working?
  - ssh-agent, gnupg-agent https://yashagarwal.in/posts/2017/12/setting-up-ssh-agent-in-i3/
  - hide cursor
  - alt+c alt+v copy/paste
- [Openbox](http://openbox.org/wiki/Main_Page)
- [XMonad](http://xmonad.org)
- [awesome WM](https://awesomewm.org)
- [dwm](https://dwm.suckless.org)
