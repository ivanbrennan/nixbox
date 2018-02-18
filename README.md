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

## Channel
I'm switching from nixos-stable to nixos-unstable. There are too many bug-fixes I want, which have been merged into master but won't make it into the stable channel for a long time.
For example, https://github.com/NixOS/nixpkgs/pull/32203

**Subscribe to nixos-unstable**
```
# switch to unstable
sudo nix-channel --remove nixos
sudo nix-channel --add https://nixos.org/channels/nixos-unstable nixos
sudo nix-channel --update

# switch to stable
sudo nix-channel --remove nixos
sudo nix-channel --add https://nixos.org/channels/nixos-17.09 nixos
sudo nix-channel --update
```
