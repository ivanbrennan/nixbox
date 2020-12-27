## make

``` sh
sudo make -C /etc/nixos machine=MACHINE
```

## Todo
- xmonad
  - [x] recompile from /etc/nixos/services/xserver/xmonad.hs
    1. modify /etc/nixos/services/xserver/xmonad.hs
    2. run `nixos-rebuild switch`
    3. hit `(mod4Mask, xK_q)`
      * runs with config from /etc/nixos/services/xserver/xmonad.hs
  - [x] recompile from ~/.xmonad/xmonad.hs
    1. modify ~/.xmonad/xmonad.hs
    2. compileRestart
      * runs with config from ~/.xmonad/xmonad.hs
  - [x] without putting ghc in systemPackages
  - [x] return to config from /etc/nixos/services/xserver/xmonad.hs
  - [x] without xmonad.state?
- fix fzf preview: https://github.com/junegunn/fzf.vim/issues/751
Try some window managers
- [XMonad](http://xmonad.org)
  - can i use something like dmenu to jump to an existing application/workspace?
  - gtk themes?
  - conky, clock, calendar
- [bspwm](https://github.com/baskerville/bspwm)
- [Openbox](http://openbox.org/wiki/Main_Page)
- [awesome WM](https://awesomewm.org)
- [dwm](https://dwm.suckless.org)
