## Todo
Try some window managers
- [i3](https://i3wm.org)
- [Openbox](http://openbox.org/wiki/Main_Page)
- [XMonad](http://xmonad.org)
- [awesome WM](https://awesomewm.org)
- [dwm](https://dwm.suckless.org)


## Workarounds
[gdm does not follow xserver keymap attributes](https://github.com/NixOS/nixpkgs/issues/14318) (e.g. `services.xserver.xkbOptions`).
- script desired gsettings in `~/Development/resources/dotfiles/X11/gsettings`, including:
``` sh
gsettings set org.gnome.desktop.input-sources xkb-options \
  "['caps:ctrl_modifier', 'shift:both_capslock']"
```
