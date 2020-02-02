## make

``` sh
sudo make -C /etc/nixos machine=MACHINE
```

## Todo
Try some window managers
- [i3](https://i3wm.org)
  - set $mod to super
  - change lightdm background image
  - ssh-agent, gnupg-agent https://yashagarwal.in/posts/2017/12/setting-up-ssh-agent-in-i3/
  - hide cursor
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
  - ssh-agent not running
  - gnome-keyring is running? who ran it? why isn't it handling ssh keys?
    `desktopManager.gnome3.enable = true;`
    ```
    grep keyring /etc/pam.d/login
    auth optional /nix/store/yhnjbwazdyqc37mrgcp3jbyhsyy6s6b2-gnome-keyring-3.31.91/lib/security/pam_gnome_keyring.so
    password optional /nix/store/yhnjbwazdyqc37mrgcp3jbyhsyy6s6b2-gnome-keyring-3.31.91/lib/security/pam_gnome_keyring.so use_authtok
    session optional /nix/store/yhnjbwazdyqc37mrgcp3jbyhsyy6s6b2-gnome-keyring-3.31.91/lib/security/pam_gnome_keyring.so auto_start
    ```
    ```
    psaux bin/X
    USER       PID %CPU %MEM    VSZ   RSS TTY      STAT START   TIME COMMAND
    root      7070  0.5  0.6 524308 55756 tty7     Ssl+ 00:18   4:17 /nix/store/0x819sd3hbkb3k5xcs5jl58zgid4kmik-xorg-server-1.20.5/bin/X -config /nix/store/gf27brhsisc74s1dysh7a4qxxw90npz0-xserver.conf -xkbdir /nix/store/w2w29w2jc4z8mk3zxg71v66l997y2azx-xkeyboard-config-2.27/etc/X11/xkb -logfile /dev/null -verbose 3 -nolisten tcp -ardelay 200 -arinterval 30 -terminate -logfile /var/log/X.0.log :0 -seat seat0 -auth /var/run/lightdm/root/:0 -nolisten tcp vt7 -novtswitch
    ```
  - easily logout / lock screen?
  - gtk themes?
  - configure urxvt and/or alacritty
  - configure i3 hjkl
  - xcape stopped working?
  - alt+c alt+v copy/paste
- [Openbox](http://openbox.org/wiki/Main_Page)
- [XMonad](http://xmonad.org)
- [awesome WM](https://awesomewm.org)
- [dwm](https://dwm.suckless.org)

# xcape issue
- occasionally caps stops acting as esc

- systemctl --user status xcape
  ● xcape.service - Xcape: Ctrl is Esc when pressed alone
     Loaded: loaded (/nix/store/sllmqyks58v0xaapw09f5s6m8znccq25-unit-xcape.service/xcape.service; enabled; vendor preset: enabled)
     Active: active (running) since Fri 2020-01-31 08:59:07 EST; 17min ago
    Process: 27171 ExecStart=/nix/store/lbzz6gbyhqnfsggn5acpgzncx085zj2a-xcape-unstable-20180301/bin/xcape (code=exited, status=0/SUCCESS)
   Main PID: 27172 (xcape)
     CGroup: /user.slice/user-1000.slice/user@1000.service/xcape.service
             └─27172 /nix/store/lbzz6gbyhqnfsggn5acpgzncx085zj2a-xcape-unstable-20180301/bin/xcape

Jan 31 08:59:07 nixosbox systemd[12109]: Starting Xcape: Ctrl is Esc when pressed alone...
Jan 31 08:59:07 nixosbox systemd[12109]: Started Xcape: Ctrl is Esc when pressed alone.

- journalctl --user -f -u xcape
  Jan 31 08:59:05 nixosbox systemd[12109]: Starting Xcape: Ctrl is Esc when pressed alone...
  Jan 31 08:59:05 nixosbox xcape[27106]: Invalid MIT-MAGIC-COOKIE-1 keyInvalid MIT-MAGIC-COOKIE-1 keyUnable to connect to X11 display. Is $DISPLAY set?
  Jan 31 08:59:05 nixosbox systemd[12109]: xcape.service: Control process exited, code=exited, status=1/FAILURE
  Jan 31 08:59:05 nixosbox systemd[12109]: xcape.service: Failed with result 'exit-code'.
  Jan 31 08:59:05 nixosbox systemd[12109]: Failed to start Xcape: Ctrl is Esc when pressed alone.
  Jan 31 08:59:07 nixosbox systemd[12109]: xcape.service: Service RestartSec=2s expired, scheduling restart.
  Jan 31 08:59:07 nixosbox systemd[12109]: xcape.service: Scheduled restart job, restart counter is at 8.
  Jan 31 08:59:07 nixosbox systemd[12109]: Stopped Xcape: Ctrl is Esc when pressed alone.
  Jan 31 08:59:07 nixosbox systemd[12109]: Starting Xcape: Ctrl is Esc when pressed alone...
  Jan 31 08:59:07 nixosbox systemd[12109]: Started Xcape: Ctrl is Esc when pressed alone.

- strace the running xcape process, i can see it receiving keypresses.
