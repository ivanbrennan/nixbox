# TODO

- update nixos-hardware, but check what changes that brings
  - trace: warning: The option `services.xserver.libinput.enable' defined in `/nix/store/dibza8s8d6ggpdwjq6yk5an0yrr3kvyq-source/common/pc' has been renamed to `services.libinput.enable'.
  - trace: warning: The option `hardware.opengl.extraPackages' defined in `/nix/store/dibza8s8d6ggpdwjq6yk5an0yrr3kvyq-source/common/cpu/intel' has been renamed to `hardware.graphics.extraPackages'.
- upgrade to postgresql_16

## alacritty

alacritty.yml deprecated, please migrate to TOML using `alacritty migrate`
```
[0.000802404s] [WARN ] [alacritty] YAML config "/nix/store/yfzxh5gf53wxws04c1ah33lll8r460a2-alacritty.yml" is deprecated, please migrate to TOML using `alacritty migrate`
[0.011668641s] [WARN ] [alacritty_config_derive] Config warning: draw_bold_text_with_bright_colors has been deprecated; use colors.draw_bold_text_with_bright_colors instead
[0.012484311s] [ERROR] [alacritty_config_derive] Config error: unknown variant `Grave`, expected one of `Alt`, `AltGraph`, `CapsLock`, `Control`, `Fn`, `FnLock`, `NumLock`, `ScrollLock`, `Shift`, `Symbol`, `SymbolLock`, `Meta`, `Hyper`, `Super`, `Enter`, `Tab`, `Space`, `ArrowDown`, `ArrowLeft`, `ArrowRight`, `ArrowUp`, `End`, `Home`, `PageDown`, `PageUp`, `Backspace`, `Clear`, `Copy`, `CrSel`, `Cut`, `Delete`, `EraseEof`, `ExSel`, `Insert`, `Paste`, `Redo`, `Undo`, `Accept`, `Again`, `Attn`, `Cancel`, `ContextMenu`, `Escape`, `Execute`, `Find`, `Help`, `Pause`, `Play`, `Props`, `Select`, `ZoomIn`, `ZoomOut`, `BrightnessDown`, `BrightnessUp`, `Eject`, `LogOff`, `Power`, `PowerOff`, `PrintScreen`, `Hibernate`, `Standby`, `WakeUp`, `AllCandidates`, `Alphanumeric`, `CodeInput`, `Compose`, `Convert`, `FinalMode`, `GroupFirst`, `GroupLast`, `GroupNext`, `GroupPrevious`, `ModeChange`, `NextCandidate`, `NonConvert`, `PreviousCandidate`, `Process`, `SingleCandidate`, `HangulMode`, `HanjaMode`, `JunjaMode`, `Eisu`, `Hankaku`, `Hiragana`, `HiraganaKatakana`, `KanaMode`, `KanjiMode`, `Katakana`, `Romaji`, `Zenkaku`, `ZenkakuHankaku`, `Soft1`, `Soft2`, `Soft3`, `Soft4`, `ChannelDown`, `ChannelUp`, `Close`, `MailForward`, `MailReply`, `MailSend`, `MediaClose`, `MediaFastForward`, `MediaPause`, `MediaPlay`, `MediaPlayPause`, `MediaRecord`, `MediaRewind`, `MediaStop`, `MediaTrackNext`, `MediaTrackPrevious`, `New`, `Open`, `Print`, `Save`, `SpellCheck`, `Key11`, `Key12`, `AudioBalanceLeft`, `AudioBalanceRight`, `AudioBassBoostDown`, `AudioBassBoostToggle`, `AudioBassBoostUp`, `AudioFaderFront`, `AudioFaderRear`, `AudioSurroundModeNext`, `AudioTrebleDown`, `AudioTrebleUp`, `AudioVolumeDown`, `AudioVolumeUp`, `AudioVolumeMute`, `MicrophoneToggle`, `MicrophoneVolumeDown`, `MicrophoneVolumeUp`, `MicrophoneVolumeMute`, `SpeechCorrectionList`, `SpeechInputToggle`, `LaunchApplication1`, `LaunchApplication2`, `LaunchCalendar`, `LaunchContacts`, `LaunchMail`, `LaunchMediaPlayer`, `LaunchMusicPlayer`, `LaunchPhone`, `LaunchScreenSaver`, `LaunchSpreadsheet`, `LaunchWebBrowser`, `LaunchWebCam`, `LaunchWordProcessor`, `BrowserBack`, `BrowserFavorites`, `BrowserForward`, `BrowserHome`, `BrowserRefresh`, `BrowserSearch`, `BrowserStop`, `AppSwitch`, `Call`, `Camera`, `CameraFocus`, `EndCall`, `GoBack`, `GoHome`, `HeadsetHook`, `LastNumberRedial`, `Notification`, `MannerMode`, `VoiceDial`, `TV`, `TV3DMode`, `TVAntennaCable`, `TVAudioDescription`, `TVAudioDescriptionMixDown`, `TVAudioDescriptionMixUp`, `TVContentsMenu`, `TVDataService`, `TVInput`, `TVInputComponent1`, `TVInputComponent2`, `TVInputComposite1`, `TVInputComposite2`, `TVInputHDMI1`, `TVInputHDMI2`, `TVInputHDMI3`, `TVInputHDMI4`, `TVInputVGA1`, `TVMediaContext`, `TVNetwork`, `TVNumberEntry`, `TVPower`, `TVRadioService`, `TVSatellite`, `TVSatelliteBS`, `TVSatelliteCS`, `TVSatelliteToggle`, `TVTerrestrialAnalog`, `TVTerrestrialDigital`, `TVTimer`, `AVRInput`, `AVRPower`, `ColorF0Red`, `ColorF1Green`, `ColorF2Yellow`, `ColorF3Blue`, `ColorF4Grey`, `ColorF5Brown`, `ClosedCaptionToggle`, `Dimmer`, `DisplaySwap`, `DVR`, `Exit`, `FavoriteClear0`, `FavoriteClear1`, `FavoriteClear2`, `FavoriteClear3`, `FavoriteRecall0`, `FavoriteRecall1`, `FavoriteRecall2`, `FavoriteRecall3`, `FavoriteStore0`, `FavoriteStore1`, `FavoriteStore2`, `FavoriteStore3`, `Guide`, `GuideNextDay`, `GuidePreviousDay`, `Info`, `InstantReplay`, `Link`, `ListProgram`, `LiveContent`, `Lock`, `MediaApps`, `MediaAudioTrack`, `MediaLast`, `MediaSkipBackward`, `MediaSkipForward`, `MediaStepBackward`, `MediaStepForward`, `MediaTopMenu`, `NavigateIn`, `NavigateNext`, `NavigateOut`, `NavigatePrevious`, `NextFavoriteChannel`, `NextUserProfile`, `OnDemand`, `Pairing`, `PinPDown`, `PinPMove`, `PinPToggle`, `PinPUp`, `PlaySpeedDown`, `PlaySpeedReset`, `PlaySpeedUp`, `RandomToggle`, `RcLowBattery`, `RecordSpeedNext`, `RfBypass`, `ScanChannelsToggle`, `ScreenModeNext`, `Settings`, `SplitScreenToggle`, `STBInput`, `STBPower`, `Subtitle`, `Teletext`, `VideoModeNext`, `Wink`, `ZoomToggle`, `F1`, `F2`, `F3`, `F4`, `F5`, `F6`, `F7`, `F8`, `F9`, `F10`, `F11`, `F12`, `F13`, `F14`, `F15`, `F16`, `F17`, `F18`, `F19`, `F20`, `F21`, `F22`, `F23`, `F24`, `F25`, `F26`, `F27`, `F28`, `F29`, `F30`, `F31`, `F32`, `F33`, `F34`, `F35`
                                                 
                                                 
                                                 ; ignoring binding
[0.012542669s] [ERROR] [alacritty_config_derive] Config error: unknown variant `Grave`, expected one of `Alt`, `AltGraph`, `CapsLock`, `Control`, `Fn`, `FnLock`, `NumLock`, `ScrollLock`, `Shift`, `Symbol`, `SymbolLock`, `Meta`, `Hyper`, `Super`, `Enter`, `Tab`, `Space`, `ArrowDown`, `ArrowLeft`, `ArrowRight`, `ArrowUp`, `End`, `Home`, `PageDown`, `PageUp`, `Backspace`, `Clear`, `Copy`, `CrSel`, `Cut`, `Delete`, `EraseEof`, `ExSel`, `Insert`, `Paste`, `Redo`, `Undo`, `Accept`, `Again`, `Attn`, `Cancel`, `ContextMenu`, `Escape`, `Execute`, `Find`, `Help`, `Pause`, `Play`, `Props`, `Select`, `ZoomIn`, `ZoomOut`, `BrightnessDown`, `BrightnessUp`, `Eject`, `LogOff`, `Power`, `PowerOff`, `PrintScreen`, `Hibernate`, `Standby`, `WakeUp`, `AllCandidates`, `Alphanumeric`, `CodeInput`, `Compose`, `Convert`, `FinalMode`, `GroupFirst`, `GroupLast`, `GroupNext`, `GroupPrevious`, `ModeChange`, `NextCandidate`, `NonConvert`, `PreviousCandidate`, `Process`, `SingleCandidate`, `HangulMode`, `HanjaMode`, `JunjaMode`, `Eisu`, `Hankaku`, `Hiragana`, `HiraganaKatakana`, `KanaMode`, `KanjiMode`, `Katakana`, `Romaji`, `Zenkaku`, `ZenkakuHankaku`, `Soft1`, `Soft2`, `Soft3`, `Soft4`, `ChannelDown`, `ChannelUp`, `Close`, `MailForward`, `MailReply`, `MailSend`, `MediaClose`, `MediaFastForward`, `MediaPause`, `MediaPlay`, `MediaPlayPause`, `MediaRecord`, `MediaRewind`, `MediaStop`, `MediaTrackNext`, `MediaTrackPrevious`, `New`, `Open`, `Print`, `Save`, `SpellCheck`, `Key11`, `Key12`, `AudioBalanceLeft`, `AudioBalanceRight`, `AudioBassBoostDown`, `AudioBassBoostToggle`, `AudioBassBoostUp`, `AudioFaderFront`, `AudioFaderRear`, `AudioSurroundModeNext`, `AudioTrebleDown`, `AudioTrebleUp`, `AudioVolumeDown`, `AudioVolumeUp`, `AudioVolumeMute`, `MicrophoneToggle`, `MicrophoneVolumeDown`, `MicrophoneVolumeUp`, `MicrophoneVolumeMute`, `SpeechCorrectionList`, `SpeechInputToggle`, `LaunchApplication1`, `LaunchApplication2`, `LaunchCalendar`, `LaunchContacts`, `LaunchMail`, `LaunchMediaPlayer`, `LaunchMusicPlayer`, `LaunchPhone`, `LaunchScreenSaver`, `LaunchSpreadsheet`, `LaunchWebBrowser`, `LaunchWebCam`, `LaunchWordProcessor`, `BrowserBack`, `BrowserFavorites`, `BrowserForward`, `BrowserHome`, `BrowserRefresh`, `BrowserSearch`, `BrowserStop`, `AppSwitch`, `Call`, `Camera`, `CameraFocus`, `EndCall`, `GoBack`, `GoHome`, `HeadsetHook`, `LastNumberRedial`, `Notification`, `MannerMode`, `VoiceDial`, `TV`, `TV3DMode`, `TVAntennaCable`, `TVAudioDescription`, `TVAudioDescriptionMixDown`, `TVAudioDescriptionMixUp`, `TVContentsMenu`, `TVDataService`, `TVInput`, `TVInputComponent1`, `TVInputComponent2`, `TVInputComposite1`, `TVInputComposite2`, `TVInputHDMI1`, `TVInputHDMI2`, `TVInputHDMI3`, `TVInputHDMI4`, `TVInputVGA1`, `TVMediaContext`, `TVNetwork`, `TVNumberEntry`, `TVPower`, `TVRadioService`, `TVSatellite`, `TVSatelliteBS`, `TVSatelliteCS`, `TVSatelliteToggle`, `TVTerrestrialAnalog`, `TVTerrestrialDigital`, `TVTimer`, `AVRInput`, `AVRPower`, `ColorF0Red`, `ColorF1Green`, `ColorF2Yellow`, `ColorF3Blue`, `ColorF4Grey`, `ColorF5Brown`, `ClosedCaptionToggle`, `Dimmer`, `DisplaySwap`, `DVR`, `Exit`, `FavoriteClear0`, `FavoriteClear1`, `FavoriteClear2`, `FavoriteClear3`, `FavoriteRecall0`, `FavoriteRecall1`, `FavoriteRecall2`, `FavoriteRecall3`, `FavoriteStore0`, `FavoriteStore1`, `FavoriteStore2`, `FavoriteStore3`, `Guide`, `GuideNextDay`, `GuidePreviousDay`, `Info`, `InstantReplay`, `Link`, `ListProgram`, `LiveContent`, `Lock`, `MediaApps`, `MediaAudioTrack`, `MediaLast`, `MediaSkipBackward`, `MediaSkipForward`, `MediaStepBackward`, `MediaStepForward`, `MediaTopMenu`, `NavigateIn`, `NavigateNext`, `NavigateOut`, `NavigatePrevious`, `NextFavoriteChannel`, `NextUserProfile`, `OnDemand`, `Pairing`, `PinPDown`, `PinPMove`, `PinPToggle`, `PinPUp`, `PlaySpeedDown`, `PlaySpeedReset`, `PlaySpeedUp`, `RandomToggle`, `RcLowBattery`, `RecordSpeedNext`, `RfBypass`, `ScanChannelsToggle`, `ScreenModeNext`, `Settings`, `SplitScreenToggle`, `STBInput`, `STBPower`, `Subtitle`, `Teletext`, `VideoModeNext`, `Wink`, `ZoomToggle`, `F1`, `F2`, `F3`, `F4`, `F5`, `F6`, `F7`, `F8`, `F9`, `F10`, `F11`, `F12`, `F13`, `F14`, `F15`, `F16`, `F17`, `F18`, `F19`, `F20`, `F21`, `F22`, `F23`, `F24`, `F25`, `F26`, `F27`, `F28`, `F29`, `F30`, `F31`, `F32`, `F33`, `F34`, `F35`
                                                 
                                                 
                                                 ; ignoring binding
[0.012797836s] [WARN ] [alacritty_config_derive] Config warning: key_bindings has been deprecated; use keyboard.bindings instead
[0.012812490s] [WARN ] [alacritty_config_derive] Unused config key: double_click
[0.012821623s] [WARN ] [alacritty_config_derive] Unused config key: triple_click
[0.012849737s] [WARN ] [alacritty_config_derive] Config warning: mouse_bindings has been deprecated; use mouse.bindings instead
[0.012886978s] [WARN ] [alacritty_config_derive] Unused config key: color_schemes
```

## xmonad / pass-otp

- XMonad.Prompt.Pass: Added passOTPTypePrompt to type out one-time-passwords via xdotool.

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

## secrets management

https://github.com/tadfisher/pass-otp
- https://github.com/Misterio77/nix-config/blob/5d4e7099910905708029a2fc52eb321644af6ac3/overlays/pass-otp-fix-completion.patch#L4

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
