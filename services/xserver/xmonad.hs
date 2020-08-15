{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad ((>=>))
import Data.Bits ((.|.))
import Data.Bool (bool)
import Data.Default (def)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Monoid (All)
import Graphics.X11
  ( Button, KeyMask, KeySym, Window, controlMask, mod4Mask, noModMask, shiftMask,
    xK_1, xK_9, xK_Alt_L, xK_Alt_R, xK_Down, xK_Left, xK_Print, xK_Return, xK_Right,
    xK_Tab, xK_Up, xK_b, xK_c, xK_comma, xK_d, xK_e, xK_grave, xK_h, xK_j, xK_k,
    xK_l, xK_m, xK_o, xK_p, xK_period, xK_q, xK_r, xK_slash, xK_space, xK_t, xK_u,
    xK_v, xK_w, xK_z,
  )
import Graphics.X11.ExtraTypes
  ( xF86XK_AudioLowerVolume, xF86XK_AudioMute, xF86XK_AudioRaiseVolume, xF86XK_Copy,
    xF86XK_MonBrightnessDown, xF86XK_MonBrightnessUp, xF86XK_Paste,
  )
import Graphics.X11.Xlib.Extras (Event)
import System.Exit (exitSuccess)
import XMonad
  ( ChangeLayout (NextLayout), Choose, Full (Full), IncMasterN (IncMasterN), Layout,
    ManageHook, Mirror, Resize (Expand, Shrink), Tall, WindowSet, WorkspaceId, X,
    XConfig (XConfig), className, clickJustFocuses, composeAll, doF, doFloat, doIgnore,
    focusedBorderColor, gets, handleEventHook, io, keys, kill, layoutHook, logHook,
    manageHook, modMask, mouseBindings, normalBorderColor, resource, runQuery,
    screenWorkspace, sendMessage, setLayout, spawn, startupHook, terminal, whenJust,
    windows, windowset, withFocused, withWindowSet, workspaces, xmonad, (-->), (=?),
    (|||),
  )
import XMonad.Actions.CycleRecentWS (cycleWindowSets)
import XMonad.Actions.CycleWS (Direction1D (Next, Prev), WSType (NonEmptyWS), moveTo)
import XMonad.Hooks.DynamicLog
  ( ppCurrent, ppHidden, ppLayout, ppOutput, ppTitle, ppWsSep, statusBar, wrap,
    xmobarColor, xmobarPP,
  )
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks (AvoidStruts, avoidStruts, manageDocks)
import XMonad.Hooks.ManageHelpers
  ( composeOne, doCenterFloat, doFullFloat, isDialog, isFullscreen, (-?>),
  )
import XMonad.Layout.BoringWindows (BoringWindows, boringWindows, focusDown, focusUp)
import XMonad.Layout.Decoration
  ( Decoration, DefaultShrinker, Theme, activeBorderColor, activeColor, activeTextColor,
    decoHeight, decoWidth, fontName, inactiveBorderColor, inactiveColor, inactiveTextColor,
    urgentBorderColor, urgentColor, urgentTextColor,
  )
import XMonad.Layout.Fullscreen (FullscreenFloat, fullscreenFloat {-- , fullscreenManageHook --})
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.NoBorders (SmartBorder, smartBorders)
import XMonad.Layout.Simplest (Simplest (Simplest))
import XMonad.Layout.SubLayouts
  ( GroupMsg (MergeAll, UnMerge, UnMergeAll), Sublayout, onGroup, pullGroup, subLayout, toSubl,
  )
import XMonad.Layout.Tabbed (TabbedDecoration, addTabs, shrinkText)
import XMonad.Layout.ToggleLayouts (ToggleLayout (ToggleLayout), ToggleLayouts, toggleLayouts)
import XMonad.Layout.WindowNavigation (Navigate (Go), WindowNavigation, windowNavigation)
import XMonad.Prompt
  ( XPConfig, XPPosition (Top), alwaysHighlight, bgColor, fgColor, font, height, position,
    promptBorderWidth,
  )
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.StackSet
  ( current, focus, focusDown', focusUp', hidden, shift, sink, stack, swapDown,
    swapMaster, swapUp, tag, view, visible, workspace,
  )
import XMonad.Util.Paste (sendKey)
import XMonad.Util.Types (Direction2D (D, L, R, U))


-- https://github.com/xmonad/X11/blob/6e5ef8019a0cc49e18410a335dbdeea87b7c4aac/Graphics/X11/Types.hsc
-- https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Util-Paste.html
-- https://stackoverflow.com/questions/6605399/how-can-i-set-an-action-to-occur-on-a-key-release-in-xmonad

keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {modMask}) =
  M.fromList $
    [ -- layout algorithms
      ( (mod4Mask, xK_space),
        sendMessage NextLayout
      ),
      ( (modMask .|. controlMask, xK_space),
        sendMessage ToggleLayout
      ),
      -- workspaces
      ( (modMask, xK_period),
        moveTo Next NonEmptyWS
      ),
      ( (modMask, xK_comma),
        moveTo Prev NonEmptyWS
      ),
      ( (modMask, xK_l),
        toggleRecentWS
      ),
      ( (modMask, xK_Tab),
        cycleWindowSets recentWS [xK_Alt_L, xK_Alt_R] xK_Tab xK_grave
      ),
      -- sublayouts
      ( (mod4Mask .|. modMask, xK_h),
        sendMessage (pullGroup L)
      ),
      ( (mod4Mask .|. modMask, xK_l),
        sendMessage (pullGroup R)
      ),
      ( (mod4Mask .|. modMask, xK_k),
        sendMessage (pullGroup U)
      ),
      ( (mod4Mask .|. modMask, xK_j),
        sendMessage (pullGroup D)
      ),
      ( (mod4Mask .|. modMask .|. shiftMask, xK_m),
        withFocused (sendMessage . MergeAll)
      ),
      ( (mod4Mask .|. modMask .|. shiftMask, xK_u),
        withFocused (sendMessage . UnMergeAll)
      ),
      ( (mod4Mask .|. modMask, xK_u),
        withFocused (sendMessage . UnMerge)
      ),
      ( (mod4Mask, xK_Tab),
        onGroup focusDown'
      ),
      ( (mod4Mask .|. modMask, xK_period),
        onGroup focusDown'
      ),
      ( (mod4Mask .|. modMask, xK_comma),
        onGroup focusUp'
      ),
      ( (mod4Mask .|. modMask, xK_space),
        toSubl NextLayout
      ),
      -- focus
      ( (modMask, xK_j),
        focusDown
      ),
      ( (modMask, xK_k),
        focusUp
      ),
      ( (mod4Mask, xK_Right),
        sendMessage (Go R)
      ),
      ( (mod4Mask, xK_Left),
        sendMessage (Go L)
      ),
      ( (mod4Mask, xK_Up),
        sendMessage (Go U)
      ),
      ( (mod4Mask, xK_Down),
        sendMessage (Go D)
      ),
      -- swap
      ( (mod4Mask .|. shiftMask, xK_Return),
        windows swapMaster
      ),
      ( (mod4Mask .|. shiftMask, xK_j),
        windows swapDown
      ),
      ( (mod4Mask .|. shiftMask, xK_k),
        windows swapUp
      ),
      -- resize
      ( (modMask .|. controlMask .|. shiftMask, xK_h),
        sendMessage Shrink
      ),
      ( (modMask .|. controlMask .|. shiftMask, xK_l),
        sendMessage Expand
      ),
      -- increment/decrement master area
      ( (modMask .|. controlMask .|. shiftMask, xK_comma),
        sendMessage (IncMasterN 1)
      ),
      ( (modMask .|. controlMask .|. shiftMask, xK_period),
        sendMessage (IncMasterN (-1))
      ),
      -- refresh
      ( (modMask .|. shiftMask, xK_r),
        setLayout (layoutHook conf)
      ),
      -- tile
      ( (modMask, xK_t),
        withFocused $ windows . sink
      ),
      -- quit or restart
      ( (mod4Mask .|. shiftMask, xK_q),
        confirmPrompt xPConfig "exit" (io exitSuccess)
      ),
      ( (mod4Mask, xK_q),
        spawn "xmonad --recompile && xmonad --restart"
      ),
      -- launch/kill
      ( (modMask .|. controlMask, xK_o),
        spawn (terminal conf)
      ),
      ( (modMask, xK_space),
        spawn "dmenu_run -fn monospace:size=12 -l 24 -i -nb '#1c1c1c' -nf '#a5adb7' -sb '#222222' -sf '#ffffff'"
      ),
      ( (modMask .|. shiftMask, xK_z),
        spawn "i3lock --color=1d1d1d"
      ),
      ( (noModMask, xK_Print),
        spawn "screenshot"
      ),
      ( (controlMask, xK_Print),
        spawn "screenshot -c"
      ),
      ( (shiftMask, xK_Print),
        spawn "screenshot -a"
      ),
      ( (controlMask .|. shiftMask, xK_Print),
        spawn "screenshot -a -c"
      ),
      ( (modMask .|. shiftMask, xK_d),
        kill
      ),
      ( (modMask .|. shiftMask, xK_p),
        spawn "passmenu -fn monospace:size=12 -l 24 -i -nb '#1c1c1c' -nf '#a5adb7' -sb '#222222' -sf '#ffffff'"
      ),
      -- volume
      ( (noModMask, xF86XK_AudioRaiseVolume),
        spawn "pactl set-sink-mute @DEFAULT_SINK@ 0 && pactl set-sink-volume @DEFAULT_SINK@ +2%"
      ),
      ( (noModMask, xF86XK_AudioLowerVolume),
        spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%"
      ),
      ( (noModMask, xF86XK_AudioMute),
        spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"
      ),
      ( (mod4Mask, xF86XK_AudioMute),
        spawn "pactl set-source-mute @DEFAULT_SOURCE@ toggle"
      ),
      ( (mod4Mask, xF86XK_AudioRaiseVolume),
        spawn "pactl set-source-mute @DEFAULT_SOURCE@ 0 && pactl set-source-volume @DEFAULT_SOURCE@ +2%"
      ),
      ( (mod4Mask, xF86XK_AudioLowerVolume),
        spawn "pactl set-source-volume @DEFAULT_SOURCE@ -2%"
      ),
      -- brightness
      ( (noModMask, xF86XK_MonBrightnessUp),
        spawn "light -A 20"
      ),
      ( (noModMask, xF86XK_MonBrightnessDown),
        spawn "light -U 20"
      ),
      -- copy/paste
      ( (modMask, xK_c),
        ifTerminal (sendKey noModMask xF86XK_Copy)  (sendKey controlMask xK_c)
      ),
      ( (modMask, xK_v),
        ifTerminal (sendKey noModMask xF86XK_Paste) (sendKey controlMask xK_v)
      )
    ]
      ++ workspaceTagKeys
      ++ screenKeys
  where
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    workspaceTagKeys =
      [ ( (m .|. modMask, k),
          windows (f i)
        )
        | (k, i) <- zip [xK_1..xK_9] (workspaces conf),
          (m, f) <- [(noModMask, view), (shiftMask, shift)]
      ]

    -- super-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- super-shift-{w,e,r}, Move client to screen 1, 2, or 3
    screenKeys =
      [ ( (m .|. mod4Mask, key),
          screenWorkspace sc >>= flip whenJust (windows . f)
        )
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..],
          (f, m) <- [(view, noModMask), (shift, shiftMask)]
      ]

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

    ifTerminal :: X () -> X () -> X ()
    ifTerminal thenX elseX =
      withFocused $ isTerminal >=> bool elseX thenX

    isTerminal :: Window -> X Bool
    isTerminal =
      fmap (== "Alacritty") . runQuery className


mouseBindings' :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
mouseBindings' conf@(XConfig {modMask}) =
  M.union bindings (mouseBindings def conf)
  where
    bindings :: M.Map (KeyMask, Button) (Window -> X ())
    bindings = M.fromList $
      [
      ]


xPConfig :: XPConfig
xPConfig =
  def
    { position          = Top,
      height            = 25,
      alwaysHighlight   = False,
      promptBorderWidth = 0,
      bgColor           = "#1c1c1c",
      fgColor           = "#a5adb7",
      font              = "xft:monospace:size=12"
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
manageHook' =
  composeAll
    [ composeOne
        [ isDialog -?> doFloat,
          -- isFullscreen -?> doFullFloat,
          className =? "Gcr-prompter" -?> doCenterFloat,
          className =? "Xmessage" -?> doCenterFloat,
          -- className =? "vlc" -?> doFloat,
          resource =? "desktop_window" -?> doIgnore,
          pure True {- otherwise -} -?> doF swapDown
        ],
      -- fullscreenManageHook,
      manageDocks
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
    spawn $
      intercalate
      " | "
      [ "ps axo pid,s,command",
        "awk '/alsactl monitor default$/'",
        "xargs --no-run-if-empty kill"
      ]


layout ::
  ModifiedLayout
    WindowNavigation
    ( ModifiedLayout
        (Decoration TabbedDecoration DefaultShrinker)
        ( ModifiedLayout
            ( Sublayout
                (Choose Full Simplest)
            )
            ( ModifiedLayout
                BoringWindows
                ( ModifiedLayout
                    SmartBorder
                    ( ToggleLayouts
                        Full
                        ( ModifiedLayout
                            AvoidStruts
                            (Choose Tall (Choose (Mirror Tall) Full))
                        )
                    )
                )
            )
        )
    )
    Window
layout =
  id
    . windowNavigation
    . addTabs shrinkText theme
    . subLayout [0, 1] (Full ||| Simplest)
    . boringWindows
    . smartBorders
    . toggleLayouts Full
    -- . fullscreenFloat
    . avoidStruts
    $ layoutHook def
  where
    theme :: Theme
    theme =
      def
        { activeColor         = "#2d2d2d",
          inactiveColor       = "#353535",
          urgentColor         = "#15539e",
          activeBorderColor   = "#070707",
          inactiveBorderColor = "#1c1c1c",
          urgentBorderColor   = "#030c17",
          activeTextColor     = "#eeeeec",
          inactiveTextColor   = "#929291",
          urgentTextColor     = "#ffffff",
          fontName            = "xft:Cantarell:bold:size=10",
          decoWidth           = 200,
          decoHeight          = 22
        }


main :: IO ()
main =
  xmonad =<< statusBar "xmobar" barPP toggleStrutsKey config
  where
    config =
      ewmh $
        def
          { layoutHook         = layout,
            terminal           = "alacritty",
            clickJustFocuses   = False,
            normalBorderColor  = "#212121",
            focusedBorderColor = "#506068",
            keys               = keys',
            mouseBindings      = mouseBindings',
            manageHook         = manageHook',
            handleEventHook    = eventHook,
            logHook            = logHook',
            startupHook        = startupHook'
          }

    barPP =
      xmobarPP
        { ppCurrent = xmobarColor "#dddddd" "#004466" . wrap " " " ",
          ppHidden  = xmobarColor "#888888" "#222222" . wrap " " " ",
          ppWsSep   = "",
          ppTitle   = const "",
          ppLayout  = const ""
        }

    toggleStrutsKey XConfig {modMask} = (modMask, xK_slash)
