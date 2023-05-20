## make

``` sh
sudo make -C /etc/nixos machine=MACHINE
```

sleep battery drain
https://forums.lenovo.com/t5/Other-Linux-Discussions/X1C-gen9-fan-starts-in-sleep-mode-drains-battery-in-a-few-hours/m-p/5132432
https://bbs.archlinux.org/viewtopic.php?id=274292

## Todo
- "a stop job is running for"
  - `journalctl --boot=-1 --reverse` (search for "timed out")
  - `journalctl --user --reverse -u emacs`
pass-otp https://github.com/tadfisher/pass-otp
rtorrent https://wiki.archlinux.org/title/RTorrent
tribler https://en.wikipedia.org/wiki/Tribler
restic https://restic.net/?ref=words.filippo.io
syncthing https://docs.syncthing.net/
Try paperless
- https://github.com/paperless-ngx/paperless-ngx
Try this fix:
- https://unix.stackexchange.com/a/711592/47044
Try some window managers
- [XMonad](http://xmonad.org)
  - can i use something like dmenu to jump to an existing application/workspace?
  - gtk themes?
  - conky, clock, calendar
- [bspwm](https://github.com/baskerville/bspwm)
- [Openbox](http://openbox.org/wiki/Main_Page)
- [awesome WM](https://awesomewm.org)
- [dwm](https://dwm.suckless.org)

emacs warning:
```
GLib-GIO-CRITICAL **: 09:51:04.250: g_settings_schema_source_lookup: assertion 'source != NULL' failed
```

xmonad
- look into xmonad-contrib/XMonad/Actions/CopyWindow.hs
- look into X.A.TopicSpace and X.H.WorkspaceHistory
- workspace metadata :: [ScopeName]
  - a tmux-like session is a Scope, and the workspaces available to that Scope are determined by the workspaces' metadata
  - workspace prompt to associate a specified workspace with the current scope and position it within the current scope's workspace sort order according to a given function
    - this could be composed with additional behavior to swap the specified workspace in at the position of the currently active workspace, and dissassociate the current workspace from the current scope
- dynamic workspaces (to behave like tmux sessions)?
- prompt for directory and session-name to launch alacritty with diss running nvim

https://tools.suckless.org/dmenu/scripts/dmenu_run_with_command_history/

dirvish
- https://github.com/roginfarrer/vim-dirvish-dovish

nvim
- https://github.com/hrsh7th/nvim-cmp
- make statusline show l/L when using cnext (<M-n>), cprev, etc.
- bind live-grep to something other than g<Space>, use g<Space> for something else
- binding to grep for word under cursor and fill quickfix list
- dirvish: `p` to toggle preview window
- automatic shada save on quit and/or session management
- treesitter
  - pretty folds
- telescope add results to telescope history when populating quickfix
  - https://github.com/nvim-telescope/telescope.nvim/issues/2382
- telescope highlight filepath in results
  - https://github.com/nvim-telescope/telescope.nvim/issues/1766
- telescope extensions
  - https://github.com/rmagatti/session-lens
  - https://github.com/sindrets/diffview.nvim
  - https://github.com/HUAHUAI23/telescope-session.nvim
  - https://github.com/MrcJkb/telescope-manix
  - https://github.com/pwntester/octo.nvim
  - https://github.com/nvim-telescope/telescope-project.nvim
  - https://github.com/luc-tielen/telescope_hoogle
- lazygit

try neovide

learn helix keybindings

emacs
- treesitter

remote install/desktop/management
- https://discourse.nixos.org/t/is-there-a-way-to-do-nixos-install-on-remote-system/17318
- https://nixos.wiki/wiki/Remote_Desktop
- https://github.com/numtide/nixos-anywhere
