## make

``` sh
sudo make -C /etc/nixos machine=MACHINE
```

## Todo
Try some window managers
- [i3](https://i3wm.org)
  - gnupg-agent prompt can't break through full-screen terminal
  - can i use something like dmenu to jump to an existing application/workspace?
  - gnome-keyring prompt is ugly
  - gnupg-agent prompt is ugly
  - hand cursor is ugly
  - gtk themes?
  - urxvt alternate screen
  - urxvt BCE
  - urxvt vim cursor shape
    - doesn't work as root inside tmux
  - urxvt git commit message colors broken inside tmux
  - urxvt cursor blinks after using vim
    https://github.com/vim/vim/issues/1986
  - enable services.urxvtd?
  - conky, clock, calendar
  - alt+c alt+v copy/paste
  - clicking a link in slack opens tab in chromium but doesn't jump to it. should it?
  - replace Xft settings in Xresources with fonts.fontconfig nixos options?
- [XMonad](http://xmonad.org)
- [bspwm](https://github.com/baskerville/bspwm)
- [Openbox](http://openbox.org/wiki/Main_Page)
- [awesome WM](https://awesomewm.org)
- [dwm](https://dwm.suckless.org)
