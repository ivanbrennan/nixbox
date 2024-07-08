# TODO

## kmonad

https://github.com/kmonad/kmonad/blob/master/doc/installation.md#nixos

## sleep battery drain
https://forums.lenovo.com/t5/Other-Linux-Discussions/X1C-gen9-fan-starts-in-sleep-mode-drains-battery-in-a-few-hours/m-p/5132432
https://bbs.archlinux.org/viewtopic.php?id=274292

## Fontconfig error

```
journalctl -e --user -t xmonad
```
```
Sep 03 16:48:23 thinkpad9 xmonad[2482]: xmonad: X11 error: BadAtom (invalid Atom parameter), request code=18, error code=5
Sep 03 16:48:23 thinkpad9 xmonad[2482]: Bad _NET_WM_DESKTOP with data=[-1,0,0,0,0]
Sep 03 16:48:23 thinkpad9 xmonad[2482]: Bad _NET_WM_DESKTOP with data=[-1,0,0,0,0]
Sep 03 16:48:23 thinkpad9 xmonad[2482]: Bad _NET_WM_DESKTOP with data=[-1,0,0,0,0]
Sep 03 16:52:36 thinkpad9 xmonad[3855]: [3855:3855:0903/165236.227540:ERROR:chrome_browser_cloud_management_controller.cc(163)] Cloud management controller initialization aborted as CBCM is not enabled.
Sep 03 16:52:36 thinkpad9 xmonad[3855]: [3855:3855:0903/165236.434453:ERROR:object_proxy.cc(590)] Failed to call method: org.freedesktop.portal.Settings.Read: object_path= /org/freedesktop/portal/desktop: org.freedesktop.DBus.Error.ServiceUnknown: The name org.freedesktop.portal.Desktop was not provided by any .service files
Sep 03 16:52:43 thinkpad9 xmonad[4795]: Fontconfig error: Cannot load default config file: No such file: (null)
Sep 03 16:52:55 thinkpad9 xmonad[4972]: Fontconfig error: Cannot load default config file: No such file: (null)
Sep 03 16:52:57 thinkpad9 xmonad[5008]: Fontconfig error: Cannot load default config file: No such file: (null)
Sep 03 16:53:01 thinkpad9 xmonad[5108]: Warning: disabling flag --expose_wasm due to conflicting flags
Sep 03 16:53:04 thinkpad9 xmonad[4998]: Fontconfig error: Cannot load default config file: No such file: (null)
Sep 03 16:53:10 thinkpad9 xmonad[5179]: libpng warning: iCCP: known incorrect sRGB profile
Sep 03 16:53:10 thinkpad9 xmonad[5179]: libpng warning: iCCP: known incorrect sRGB profile
Sep 03 16:53:13 thinkpad9 xmonad[5179]: libpng warning: iCCP: known incorrect sRGB profile
Sep 03 20:47:49 thinkpad9 xmonad[29435]: Fontconfig error: Cannot load default config file: No such file: (null)
Sep 03 21:36:35 thinkpad9 xmonad[41897]: Fontconfig error: Cannot load default config file: No such file: (null)
Sep 03 21:36:42 thinkpad9 xmonad[41932]: Fontconfig error: Cannot load default config file: No such file: (null)
Sep 03 21:40:19 thinkpad9 xmonad[42135]: Fontconfig error: Cannot load default config file: No such file: (null)
Sep 03 21:41:07 thinkpad9 xmonad[42167]: Fontconfig error: Cannot load default config file: No such file: (null)
Sep 03 22:04:02 thinkpad9 xmonad[43410]: Fontconfig error: Cannot load default config file: No such file: (null)
```

## stop job

"a stop job is running for"
- `journalctl --boot=-1 --reverse` (search for "timed out")
- `journalctl --user --reverse -u emacs`

## document management

- https://github.com/paperless-ngx/paperless-ngx

## declarative configuration

- https://github.com/nix-community/impermanence
- https://github.com/nix-community/disko
- https://github.com/Misterio77/nix-starter-configs#what-next
- https://github.com/nix-community/nix-doom-emacs/blob/master/docs/reference.md#home-manager

## nix

- https://nixos.wiki/wiki/Binary_Cache#How_to_check_if_content_is_on_a_binary_cache

## vim

- https://github.com/nvim-telescope/telescope-frecency.nvim
  - https://github.com/dhruvasagar/vim-buffer-history
- https://github.com/roginfarrer/vim-dirvish-dovish
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
- neovide
- learn helix keybindings

```vim
function! JumpToPreviousBufferInJumplist()
    let initial_bufnr = bufnr('%')
    while winrestcmd('wincmd p') && bufnr('%') == initial_bufnr
        execute "normal! \<C-O>"
    endwhile
endfunction

function! JumpToNextBufferInJumplist()
    let initial_bufnr = bufnr('%')
    while winrestcmd('wincmd p') && bufnr('%') == initial_bufnr
        execute "normal! \<C-I>"
    endwhile
endfunction

nnoremap <silent> <C-S-O> :call JumpToPreviousBufferInJumplist()<CR>
nnoremap <silent> <C-S-I> :call JumpToNextBufferInJumplist()<CR>
```

## emacs

warning:
```
GLib-GIO-CRITICAL **: 09:51:04.250: g_settings_schema_source_lookup: assertion 'source != NULL' failed
```
- treesitter

## xmonad
- look into xmonad-contrib/XMonad/Actions/CopyWindow.hs
- look into X.A.TopicSpace and X.H.WorkspaceHistory
- workspace metadata :: [ScopeName]
  - a tmux-like session is a Scope, and the workspaces available to that Scope are determined by the workspaces' metadata
  - workspace prompt to associate a specified workspace with the current scope and position it within the current scope's workspace sort order according to a given function
    - this could be composed with additional behavior to swap the specified workspace in at the position of the currently active workspace, and dissassociate the current workspace from the current scope
- dynamic workspaces (to behave like tmux sessions)?
- prompt for directory and session-name to launch alacritty with diss running nvim

## window managers

- https://github.com/baskerville/bspwm
- http://openbox.org/wiki/Main_Page
- https://awesomewm.org
- https://dwm.suckless.org

## zellij

- Alt z
- https://github.com/zellij-org/zellij/issues/1399#issuecomment-1249822538
- https://zellij.dev/news/session-manager-protobuffs/
- https://www.reddit.com/r/vimporn/comments/10eqj6z/comment/j4wsfbh/?utm_source=reddit&utm_medium=web2x&context=3
  > * First of all, I dedicate one tab [exclusively for the editor](https://github.com/alex35mil/dotfiles/blob/597a5eda06960041224f56b1fd5cf07b0798005e/user/cfg/zellij/layouts/editor.kdl#L10-L12).
  > * When I work with the editor, I make sure I am in the Zellij Locked mode.
  > * Locked mode has only 2 shortcuts:
  >   * [Switch to Normal mode and go to the Terminal tab](https://github.com/alex35mil/dotfiles/blob/597a5eda06960041224f56b1fd5cf07b0798005e/user/cfg/zellij/layouts/editor.kdl#L21)
  >   * [Detach](https://github.com/alex35mil/dotfiles/blob/597a5eda06960041224f56b1fd5cf07b0798005e/user/cfg/zellij/config.kdl#L3)
  > * In Normal mode I have all the shortcuts I want since I am not inside the editor.
  > * To switch back to the editor (and to the Locked mode), I use the [same shortcut that I use to switch to the Normal mode from the editor](https://github.com/alex35mil/dotfiles/blob/597a5eda06960041224f56b1fd5cf07b0798005e/user/cfg/zellij/layouts/editor.kdl#L25). This shortcut basically toggles me between editor world and the rest of the session.
  > * And last but not least, in Locked mode, I bind keys that I would never use in Neovim, such as Insert or Home. For convenience, I remapped R key to the Insert on a separate layer of my keyboard. So to switch between modes, I hit Fn + R.
- https://github.com/alma3lol/zellij-bar
- https://github.com/alex35mil/dotfiles/tree/400d3cfef72f8b5d141eebda64718297c3bbe478/user/bin/zellij/runner

## remote install/desktop/management

- https://discourse.nixos.org/t/is-there-a-way-to-do-nixos-install-on-remote-system/17318
- https://nixos.wiki/wiki/Remote_Desktop
- https://github.com/numtide/nixos-anywhere

## misc

- https://unix.stackexchange.com/a/711592/47044
- https://wiki.archlinux.org/title/RTorrent
- https://en.wikipedia.org/wiki/Tribler
- https://restic.net/?ref=words.filippo.io
- https://docs.syncthing.net/
- https://tools.suckless.org/dmenu/scripts/dmenu_run_with_command_history/
