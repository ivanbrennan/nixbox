{-# LANGUAGE NamedFieldPuns #-}

import XMonad
    (ChangeLayout(NextLayout), Choose, Full(Full), IncMasterN(IncMasterN), Layout, ManageHook, Mirror,
     Resize(Expand, Shrink), Tall, X, XConfig(XConfig), (=?), (-->), className, clickJustFocuses,
     composeAll, doFloat, doIgnore, focusedBorderColor, handleEventHook, io, keys, kill, layoutHook,
     logHook, manageHook, modMask, normalBorderColor, resource, screenWorkspace, sendMessage, spawn,
     startupHook, terminal, whenJust, windows, withFocused, workspaces, xmonad, doF, withWindowSet, runQuery, WindowSet, WorkspaceId,
     gets, windowset
    )
import XMonad.Actions.CycleWS (Direction1D(Next, Prev), WSType(NonEmptyWS), moveTo)
import XMonad.Actions.CycleRecentWS (cycleWindowSets)
import XMonad.Hooks.DynamicLog (ppOutput, ppTitle, statusBar, xmobarColor, xmobarPP, ppCurrent, ppHidden, ppLayout, ppWsSep, wrap)
import XMonad.Hooks.ManageDocks (AvoidStruts, avoidStruts, manageDocks)
import XMonad.Hooks.ManageHelpers (composeOne, doCenterFloat, isDialog, (-?>))
import XMonad.Layout.Fullscreen (FullscreenFloat, fullscreenFloat, fullscreenEventHook, fullscreenManageHook)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.NoBorders (SmartBorder, smartBorders)
import XMonad.Layout.ToggleLayouts (ToggleLayouts, ToggleLayout(ToggleLayout), toggleLayouts)
import XMonad.Prompt (XPConfig, XPPosition(Top), alwaysHighlight, bgColor, fgColor, font, height, position, promptBorderWidth)
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.StackSet
    (focusDown, focusUp, focusMaster, shift, swapMaster, swapDown, swapUp,
     sink, greedyView, view, focus, stack, workspace, current, tag, visible, hidden
    )
import XMonad.Util.Paste (sendKey)
import Graphics.X11
    (KeyMask, KeySym, Window, controlMask, mod4Mask, noModMask, shiftMask, xK_1, xK_9, xK_b, xK_c, xK_d, xK_e, xK_h, xK_j,
     xK_k, xK_l, xK_m, xK_o, xK_r, xK_t, xK_w, xK_q, xK_v, xK_z, xK_Return, xK_comma, xK_period, xK_space,
     xK_Print, xK_Tab, xK_Alt_L, xK_Alt_R, xK_grave
    )
import Graphics.X11.ExtraTypes
    (xF86XK_AudioRaiseVolume, xF86XK_AudioLowerVolume, xF86XK_AudioMute, xF86XK_MonBrightnessUp, xF86XK_MonBrightnessDown
    )
import Graphics.X11.Xlib.Extras (Event)
import Control.Monad ((>=>))
import Data.Bits ((.|.))
import Data.Default (def)
import Data.List (intercalate)
import Data.Monoid (All)
import System.Exit (exitSuccess)

import qualified Data.Map as M


-- https://github.com/xmonad/X11/blob/6e5ef8019a0cc49e18410a335dbdeea87b7c4aac/Graphics/X11/Types.hsc
-- https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Util-Paste.html
-- https://stackoverflow.com/questions/6605399/how-can-i-set-an-action-to-occur-on-a-key-release-in-xmonad

-- This looks really powerful:
-- https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Actions-GroupNavigation.html

keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {modMask}) = M.fromList $
    -- layout algorithms
    [ ((mod4Mask,           xK_space ), sendMessage NextLayout)
    , ((modShiftMask,       xK_space ), sendMessage ToggleLayout)

    -- workspaces
    , ((modMask,            xK_period), moveTo Next NonEmptyWS)
    , ((modMask,            xK_comma ), moveTo Prev NonEmptyWS)
    , ((modMask,            xK_l     ), toggleRecentWS)
    , ((modMask,            xK_Tab   ), cycleWindowSets recentWS [xK_Alt_L, xK_Alt_R] xK_Tab xK_grave)

    -- focus
    , ((modMask,            xK_j     ), windows focusDown)
    , ((modMask,            xK_k     ), windows focusUp  )
    , ((modMask,            xK_m     ), windows focusMaster)

    -- swap
    , ((modShiftMask,       xK_Return), windows swapMaster)
    , ((modShiftMask,       xK_j     ), windows swapDown  )
    , ((modShiftMask,       xK_k     ), windows swapUp    )

    -- resize
    , ((modShiftMask,       xK_h     ), sendMessage Shrink)
    , ((modShiftMask,       xK_l     ), sendMessage Expand)

    -- increment/decrement master area
    , ((modShiftMask,       xK_comma ), sendMessage (IncMasterN 1))
    , ((modShiftMask,       xK_period), sendMessage (IncMasterN (-1)))

    -- tile
    , ((modMask,            xK_t     ), withFocused $ windows . sink)

    -- quit or restart
    , ((mod4ShiftMask,      xK_q     ), confirmPrompt xPConfig "exit" (io exitSuccess))
    , ((mod4Mask,           xK_q     ), spawn "xmonad --recompile && xmonad --restart")

    -- launch/kill
    , ((modShiftMask,       xK_o     ), spawn (terminal conf))
    , ((modMask,            xK_space ), spawn "dmenu_run -fn monospace:size=12 -l 16 -i -nb '#1c1c1c' -nf '#a5adb7' -sb '#1f1f1f' -sf '#c8f5ff'")
    , ((modShiftMask,       xK_z     ), spawn "i3lock --color=1d1d1d")
    , ((noModMask,          xK_Print ), spawn "screenshot")
    , ((controlMask,        xK_Print ), spawn "screenshot -c")
    , ((shiftMask,          xK_Print ), spawn "screenshot -a")
    , ((controlShiftMask,   xK_Print ), spawn "screenshot -a -c")
    , ((modShiftMask,       xK_d     ), kill)

    -- volume
    , ((noModMask, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%")
    , ((noModMask, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%")
    , ((noModMask, xF86XK_AudioMute       ), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")

    -- brightness
    , ((noModMask, xF86XK_MonBrightnessUp  ), spawn "light -A 10")
    , ((noModMask, xF86XK_MonBrightnessDown), spawn "light -U 10")

    -- copy/paste
    , ((modMask,            xK_c     ), clipboard xK_c)
    , ((modMask,            xK_v     ), clipboard xK_v)
    ]
    ++
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [ ((m .|. modMask, k), windows $ f i)
      | (k, i) <- zip [xK_1..xK_9] (workspaces conf)
      , (m, f) <- [(noModMask, greedyView), (shiftMask, shift)]
    ]
    ++
    -- super-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- super-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [ ((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
      , (f, m) <- [(view, noModMask), (shift, shiftMask)]
    ]
  where
    modShiftMask = modMask .|. shiftMask
    mod4ShiftMask = mod4Mask .|. shiftMask
    controlShiftMask = controlMask .|. shiftMask

    toggleRecentWS :: X ()
    toggleRecentWS =
      gets (recentWS . windowset) >>= windows . const . head

    recentWS :: WindowSet -> [WindowSet]
    recentWS ws = map (`view` ws) (recentTags ws)

    recentTags :: WindowSet -> [WorkspaceId]
    recentTags ws = map tag
      $ filter (not . null . stack)
      $ map workspace (visible ws)
        ++ hidden ws
        ++ [workspace (current ws)]

    clipboard :: KeySym -> X ()
    clipboard k =
      withFocused (clipMask >=> flip sendKey k)

    clipMask :: Window -> X KeyMask
    clipMask w = do
      name <- runQuery className w
      case name of
        "Alacritty" -> pure (controlMask .|. modMask)
        _           -> pure controlMask


xPConfig :: XPConfig
xPConfig = def
  { position          = Top
  , height            = 22
  , alwaysHighlight   = False
  , promptBorderWidth = 0
  , bgColor           = "#161616"
  , fgColor           = "#b1e0e7"
  , font              = "xft:monospace:size=11"
  }

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
manageHook' :: ManageHook
manageHook' = composeAll
    [ composeOne
      [ isDialog                      -?> doFloat
      , className =? "Gcr-prompter"   -?> doCenterFloat
      , className =? "Xmessage"       -?> doCenterFloat
      , className =? "vlc"            -?> doFloat
      , resource  =? "desktop_window" -?> doIgnore
      , pure True {- otherwise -}     -?> doF swapDown
      ]
    , fullscreenManageHook
    , manageDocks
    ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
eventHook :: Event -> X All
eventHook = fullscreenEventHook

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
logHook' :: X ()
logHook' = pure ()

------------------------------------------------------------------------
-- Startup hook

startupHook' :: X ()
startupHook' =
    -- https://github.com/jaor/xmobar/issues/432
    spawn $ intercalate " | "
      [ "ps axo pid,s,command"
      , "awk '/alsactl monitor default$/'"
      , "xargs --no-run-if-empty kill"
      ]


layout
    :: ModifiedLayout
       SmartBorder
     ( ToggleLayouts
       Full
     ( ModifiedLayout
       FullscreenFloat
     ( ModifiedLayout
       AvoidStruts
       ( Choose Tall ( Choose (Mirror Tall) Full )
       )
     )))
       Window
layout = id
    . smartBorders
    . toggleLayouts Full
    . fullscreenFloat
    . avoidStruts
    $ layoutHook def


main :: IO ()
main =
    xmonad =<< statusBar "xmobar" barPP toggleStrutsKey config
  where
    config = def
        { layoutHook         = layout
        , terminal           = "alacritty"
        , clickJustFocuses   = False
        , normalBorderColor  = "#212121"
        , focusedBorderColor = "#506068"
        , keys               = keys'
        , manageHook         = manageHook'
        , handleEventHook    = eventHook
        , logHook            = logHook'
        , startupHook        = startupHook'
        }

    barPP = xmobarPP
        { ppCurrent = xmobarColor "#dddddd" "#004466" . wrap " " " "
        , ppHidden  = xmobarColor "#888888" "#222222" . wrap " " " "
        , ppWsSep = ""
        , ppTitle = const ""
        , ppLayout = const ""
        }

    toggleStrutsKey XConfig {modMask} = (modMask .|. shiftMask, xK_b)
