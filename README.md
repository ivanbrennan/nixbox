## make

``` sh
sudo make -C /etc/nixos machine=MACHINE
```

## Todo
Try some window managers
- [i3](https://i3wm.org)
  - set $mod to super
  - change lightdm background image
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
    - [✓] desktop.background picture-uri '/run/current-system/sw/share/backgrounds/gnome/Godafoss_Iceland.jpg'
  - gnupg-agent prompt can't break through full-screen terminal
    * ultimate, however, I'd rather make the window decorations subtler and not use full-screen.
      that way i can still see the clock, battery, etc.
  - configure i3 status bar
  - can i use something like dmenu to jump to an existing application/workspace?
  - gnome-keyring prompt is ugly
  - gnupg-agent prompt is ugly
  - hand cursor is ugly
  - brightness/volume keys
  - easily logout / lock screen?
  - gtk themes?
  - urxvt alternate screen
  - urxvt BCE
  - urxvt vim cursor shape
    - doesn't work as root inside tmux
  - urxvt git commit message colors broken inside tmux
  - urxvt cursor blinks after using vim
    https://github.com/vim/vim/issues/1986
  - enable services.urxvtd?
  - configure i3 hjkl
  - alt+c alt+v copy/paste
  - clicking a link in slack opens tab in chromium but doesn't jump to it. should it?
- [Openbox](http://openbox.org/wiki/Main_Page)
- [XMonad](http://xmonad.org)
- [awesome WM](https://awesomewm.org)
- [dwm](https://dwm.suckless.org)
