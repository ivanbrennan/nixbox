{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wall -Werror #-}

{- base -}
import Control.Arrow (second)
import Control.Monad ((>=>))
import Data.Bits ((.|.))
import Data.Bool (bool)
import Data.List (intercalate, isInfixOf)
import Data.Monoid (All)
import Data.Time (ZonedTime, defaultTimeLocale, formatTime, getZonedTime)
import System.Directory (getHomeDirectory)
import System.Exit (exitSuccess)
import System.FilePath ((</>))

{- containers -}
import Data.Map (Map)
import qualified Data.Map as M

{- data-default -}
import Data.Default (def)

{- X11 -}
import Graphics.X11
  ( Button, KeyMask, KeySym, Window, button1, controlMask, mod4Mask, noModMask,
    shiftMask, xK_1, xK_9, xK_Alt_L, xK_Alt_R, xK_Print, xK_Tab, xK_a, xK_c,
    xK_comma, xK_d, xK_e, xK_g, xK_h, xK_i, xK_j, xK_k, xK_l, xK_m, xK_n, xK_p,
    xK_period, xK_q, xK_r, xK_semicolon, xK_slash, xK_space, xK_t, xK_v, xK_w,
    xK_x, xK_z,
  )
import Graphics.X11.ExtraTypes
  ( xF86XK_AudioLowerVolume, xF86XK_AudioMute, xF86XK_AudioRaiseVolume,
    xF86XK_Copy, xF86XK_MonBrightnessDown, xF86XK_MonBrightnessUp, xF86XK_Paste,
  )
import Graphics.X11.Xlib.Extras (Event)

{- xmonad -}
import XMonad
  ( ChangeLayout (FirstLayout, NextLayout), Choose, Full (Full),
    IncMasterN (IncMasterN), Layout, ManageHook, Mirror (Mirror),
    Resize (Expand, Shrink), WindowSet, WindowSpace, WorkspaceId, X, XConf,
    XConfig (XConfig), appName, className, clickJustFocuses, composeAll, config,
    description, doFloat, doIgnore, focus, focusedBorderColor, gets,
    handleEventHook, io, keys, kill, layoutHook, local, logHook, manageHook,
    modMask, mouseBindings, mouseMoveWindow, normalBorderColor, refresh, runQuery,
    screenWorkspace, sendMessage, spawn, startupHook, terminal, trace, whenJust,
    windows, windowset, withFocused, workspaces, xmonad, (=?), (|||),
  )
import qualified XMonad.StackSet as W

{- xmonad-contrib -}
import XMonad.Actions.CycleWS
  ( Direction1D (Next, Prev), WSType (WSIs, NonEmptyWS), moveTo,
  )
import XMonad.Actions.FlexibleResize (mouseResizeEdgeWindow)
import XMonad.Actions.RotSlaves (rotSlavesDown, rotSlavesUp)
import XMonad.Actions.Submap (submap)
import XMonad.Actions.WindowBringer (gotoMenuArgs)
import XMonad.Hooks.DebugStack (debugStackString)
import XMonad.Hooks.DynamicLog
  ( PP, ppCurrent, ppHidden, ppLayout, ppSep, ppTitle, ppWsSep, statusBar, wrap,
    xmobarColor, xmobarPP,
  )
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.InsertPosition
  ( Focus (Newer), Position (Above), insertPosition,
  )
import XMonad.Hooks.ManageDebug (debugManageHookOn)
import XMonad.Hooks.ManageHelpers
  ( composeOne, doCenterFloat, doRectFloat, isDialog, (-?>),
  )
import XMonad.Hooks.RefocusLast
  ( RefocusLastLayoutHook, isFloat, refocusLastLayoutHook, refocusLastWhen,
    shiftRLWhen, swapWithLast, toggleFocus,
  )
import XMonad.Layout.BoringWindows (BoringWindows, boringAuto, focusDown, focusUp)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.LimitWindows
  ( Selection, limitSelect, decreaseLimit, increaseLimit,
  )
import XMonad.Layout.NoBorders (SmartBorder, smartBorders)
import XMonad.Layout.ResizableTile
  ( ResizableTall (ResizableTall), MirrorResize (MirrorShrink, MirrorExpand),
  )
import XMonad.Layout.StateFull (FocusTracking, focusTracking)
import XMonad.Layout.ToggleLayouts
  ( ToggleLayout (ToggleLayout), ToggleLayouts, toggleLayouts,
  )
import XMonad.Prompt
  ( XPConfig, XPPosition (Top), alwaysHighlight, autoComplete, bgColor, fgColor,
    font, height, position, promptBorderWidth,
  )
import XMonad.Prompt.AppendFile (appendFilePrompt')
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.Prompt.Man (manPrompt)
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Prompt.XMonad (xmonadPromptC)
import XMonad.Util.NamedScratchpad
  ( NamedScratchpad (NS), namedScratchpadAction,
    namedScratchpadFilterOutWorkspace, namedScratchpadFilterOutWorkspacePP,
    namedScratchpadManageHook,
  )
import qualified XMonad.Util.NamedScratchpad as NS
import XMonad.Util.Paste (sendKey)


-- https://github.com/xmonad/X11/blob/6e5ef8019a0cc49e18410a335dbdeea87b7c4aac/Graphics/X11/Types.hsc
-- https://stackoverflow.com/questions/6605399/how-can-i-set-an-action-to-occur-on-a-key-release-in-xmonad

keys' :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {modMask}) =
  M.fromList $
    [ -- layout algorithms
      ( (mod4Mask, xK_space),
        sendMessage NextLayout
      ),
      ( (mod4Mask .|. shiftMask, xK_space),
        sendMessage FirstLayout
      ),
      ( (modMask, xK_space),
        sendMessage ToggleLayout
      ),
      -- workspaces
      ( (modMask, xK_period),
        moveTo Next cycledWorkspace
      ),
      ( (modMask, xK_comma),
        moveTo Prev cycledWorkspace
      ),
      ( (modMask, xK_l),
        toggleRecentWS
      ),
      ( (mod4Mask, xK_Tab),
        moveTo Next NonEmptyWS
      ),
      -- focus
      ( (modMask, xK_j),
        do
          lt <- layoutDescription
          if "Full" `isInfixOf` lt
            then windows W.focusDown
            else focusDown
      ),
      ( (modMask, xK_k),
        do
          lt <- layoutDescription
          if "Full" `isInfixOf` lt
            then windows W.focusUp
            else focusUp
      ),
      ( (modMask, xK_m),
        windows W.focusMaster
      ),
      ( (modMask, xK_semicolon),
        toggleFocus
      ),
      ( (mod4Mask, xK_semicolon),
        swapWithLast
      ),
      -- rotation
      ( (modMask .|. controlMask, xK_k),
        rotSlavesDown
      ),
      ( (modMask .|. controlMask, xK_j),
        rotSlavesUp
      ),
      ( (modMask .|. controlMask, xK_n),
        rotTailUp
      ),
      ( (modMask .|. controlMask, xK_p),
        rotTailDown
      ),
      ( (modMask .|. controlMask, xK_comma),
        increaseLimit
      ),
      ( (modMask .|. controlMask, xK_period),
        decreaseLimit
      ),
      ( (mod4Mask .|. controlMask, xK_comma),
        sendMessage (IncMasterN 1)
      ),
      ( (mod4Mask .|. controlMask, xK_period),
        sendMessage (IncMasterN (-1))
      ),
      -- swap
      ( (modMask .|. shiftMask, xK_m),
        windows W.swapMaster
      ),
      ( (modMask .|. shiftMask, xK_j),
        windows siftDown -- TODO: boring-aware
      ),
      ( (modMask .|. shiftMask, xK_k),
        windows siftUp -- TODO: boring-aware
      ),
      -- resize
      ( (mod4Mask, xK_h),
        sendMessage Shrink
      ),
      ( (mod4Mask, xK_l),
        sendMessage Expand
      ),
      ( (mod4Mask, xK_j),
        sendMessage MirrorShrink
      ),
      ( (mod4Mask, xK_k),
        sendMessage MirrorExpand
      ),
      -- refresh
      ( (mod4Mask .|. shiftMask, xK_r),
        refresh
      ),
      -- tile
      ( (mod4Mask, xK_t),
        withFocused (windows . W.sink)
      ),
      -- quit or restart
      ( (mod4Mask, xK_q),
        spawn "xmonad --recompile && xmonad --restart"
      ),
      ( (mod4Mask .|. modMask, xK_q),
        spawn $
          intercalate
            ";"
            [ "ln -s /dev/null ~/.xmonad/xmonad.state || true",
              "xmonad --recompile && xmonad --restart"
            ]
      ),
      ( (mod4Mask .|. shiftMask, xK_q),
        confirmPrompt xPConfig "exit" (io exitSuccess)
      ),
      -- launch/kill
      ( (modMask, xK_i),
        spawn (terminal conf)
      ),
      ( (controlMask, xK_space),
        spawn dmenu
      ),
      ( (controlMask .|. shiftMask, xK_space),
        namedScratchpadAction scratchpads (NS.name scratchTerminal)
      ),
      ( (mod4Mask .|. modMask, xK_p),
        debugStackString
          >>= trace . unlines . uncurry (++) . second reverse . splitAt 1 . lines
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
      ( (mod4Mask .|. shiftMask, xK_d),
        kill
      ),
      ( (modMask .|. shiftMask, xK_space),
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
      ++ [ ( (modMask, xK_slash),
             submap . M.fromList $
               [ ( (modMask, xK_slash),
                   local (setTerminal "alacritty --class=manpage") $
                     manPrompt (xPConfig {autoComplete = Just 0})
                 )
               ]
           )
         ]
      ++ [ ( (modMask, alt),
             submap . M.fromList $
               [ ( (noModMask, xK_a),
                   appendThoughtPrompt xPConfig
                 ),
                 ( (noModMask, xK_g),
                   gotoMenuArgs $
                     filter (not . (== '\'')) <$> dmenuArgs
                 ),
                 ( (noModMask, xK_r),
                   runOrRaisePrompt xPConfig
                 ),
                 ( (noModMask, xK_x),
                   xmonadPromptC commands xPConfig
                 ),
                 ( (noModMask, xK_z),
                   spawn "i3lock --color=1d1d1d"
                 )
               ]
           )
           | alt <- [xK_Alt_L, xK_Alt_R]
         ]
  where
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    workspaceTagKeys =
      [ ( (m .|. modMask, k),
          windows =<< f i
        )
        | (k, i) <- zip [xK_1..xK_9] (workspaces conf),
          (m, f) <- [ (noModMask, pure . W.view),
                      (shiftMask, shiftRLWhen isFloat)
                    ]
      ]

    -- super-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- super-shift-{w,e,r}, Move client to screen 1, 2, or 3
    screenKeys =
      [ ( (m .|. mod4Mask, key),
          screenWorkspace sc >>= flip whenJust (f >=> windows)
        )
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..],
          (f, m) <- [ (pure . W.view, noModMask),
                      (shiftRLWhen isFloat, shiftMask)
                    ]
      ]

    layoutDescription :: X String
    layoutDescription =
      gets (description . W.layout . W.workspace . W.current . windowset)

    cycledWorkspace :: WSType
    cycledWorkspace = WSIs $ pure (not . boringWorkspace)

    boringWorkspace :: WindowSpace -> Bool
    boringWorkspace ws = emptyWorkspace ws || scratchpadWorkspace ws

    emptyWorkspace :: WindowSpace -> Bool
    emptyWorkspace = null . W.stack

    scratchpadWorkspace :: WindowSpace -> Bool
    scratchpadWorkspace = null . namedScratchpadFilterOutWorkspace . (:[])

    toggleRecentWS :: X ()
    toggleRecentWS =
      gets (recentWS . windowset) >>= windows . const . head

    recentWS :: WindowSet -> [WindowSet]
    recentWS ws = map (`W.view` ws) (recentTags ws)

    recentTags :: WindowSet -> [WorkspaceId]
    recentTags ws =
      map W.tag
        . filter (not . null . W.stack)
        . namedScratchpadFilterOutWorkspace
        $ map W.workspace (W.visible ws)
          ++ W.hidden ws
          ++ [W.workspace (W.current ws)]

    siftUp :: WindowSet -> WindowSet
    siftUp = W.modify' siftUp'

    siftDown :: WindowSet -> WindowSet
    siftDown = W.modify' (reverseStack . siftUp' . reverseStack)

    siftUp' :: W.Stack Window -> W.Stack Window
    siftUp' (W.Stack t (l:ls) rs) = W.Stack t ls (l:rs)
    siftUp' (W.Stack t []     rs) =
      case reverse rs of
        []      -> W.Stack t []           []
        (r:rs') -> W.Stack t (rs' ++ [r]) []

    reverseStack :: W.Stack Window -> W.Stack Window
    reverseStack (W.Stack t ls rs) = W.Stack t rs ls

    rotTailUp :: X ()
    rotTailUp =
      windows $ W.modify' rotTailUp'

    rotTailDown :: X ()
    rotTailDown =
      windows $ W.modify' (reverseRights . rotTailUp' . reverseRights)

    rotTailUp' :: W.Stack Window -> W.Stack Window
    rotTailUp' (W.Stack t ls (r:rs)) = W.Stack r ls (rs ++ [t])
    rotTailUp' (W.Stack t ls     []) = W.Stack t ls []

    reverseRights :: W.Stack Window -> W.Stack Window
    reverseRights (W.Stack t ls rs) = W.Stack t ls (reverse rs)

    setTerminal :: String -> XConf -> XConf
    setTerminal t xc =
      xc {config = (config xc) {terminal = t}}

    ifTerminal :: X () -> X () -> X ()
    ifTerminal thenX elseX =
      withFocused $ isTerminal >=> bool elseX thenX

    isTerminal :: Window -> X Bool
    isTerminal =
      fmap (== "Alacritty") . runQuery className

    appendThoughtPrompt :: XPConfig -> X ()
    appendThoughtPrompt xP = do
      home <- io getHomeDirectory
      time <- io (fmap timestamp getZonedTime)
      appendFilePrompt' xP (time ++) (home </> "thoughts")

    timestamp :: ZonedTime -> String
    timestamp = formatTime defaultTimeLocale "[%FT%T%Ez] "

    dmenu :: String
    dmenu =
      intercalate " " ("dmenu_run" : dmenuArgs)

    dmenuArgs :: [String]
    dmenuArgs =
      [ "-fn", "monospace:size=12",
        "-l", "24",
        "-i",
        "-nb", "'#1c1c1c'",
        "-nf", "'#a5adb7'",
        "-sb", "'#222222'",
        "-sf", "'#ffffff'"
      ]

    commands :: [(String, X ())]
    commands =
      [ ("shrink", sendMessage Shrink),
        ("expand", sendMessage Expand),
        ("refresh", refresh)
      ]


mouseBindings' :: XConfig Layout -> Map (KeyMask, Button) (Window -> X ())
mouseBindings' XConfig {modMask} =
  M.fromList $
    [ ( (modMask, button1),
        \w ->
          focus w
            >> mouseMoveWindow w
            >> windows W.shiftMaster
      ),
      ( (mod4Mask, button1),
        \w ->
          focus w
            >> mouseResizeEdgeWindow (3/4) w
            >> windows W.shiftMaster
      )
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

scratchpads :: [NamedScratchpad]
scratchpads =
  [ scratchTerminal
  ]

scratchTerminal :: NamedScratchpad
scratchTerminal =
  NS name command (appName =? name) (doCenteredFloat 0.8 0.7)
  where
    name :: String
    name = "scratchpad"

    command :: String
    command = "alacritty-transparent --class " ++ name

doCenteredFloat :: Rational -> Rational -> ManageHook
doCenteredFloat width height =
  doRectFloat (W.RationalRect x y width height)
  where
    x :: Rational
    x = (1 - width) / 2

    y :: Rational
    y = (1 - height) / 2

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
-- 'className' and 'appName' are used below.
--
manageHook' :: ManageHook
manageHook' =
  composeAll
    [ insertPosition Above Newer,
      composeOne
        [ isDialog -?> doFloat,
          -- isFullscreen -?> doFullFloat,
          className =? "Gcr-prompter" -?> doCenterFloat,
          className =? "Xmessage" -?> doCenterFloat,
          -- className =? "vlc" -?> doFloat,
          appName =? "desktop_window" -?> doIgnore,
          appName =? "manpage" -?> doCenteredFloat 0.6 0.6
        ],
      -- fullscreenManageHook,
      namedScratchpadManageHook scratchpads
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
eventHook =
  refocusLastWhen isFloat <> fullscreenEventHook

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
        "awk '/alsactl monitor default$/{print $1}'",
        "xargs --no-run-if-empty kill"
      ]


type SmartBorders a = ModifiedLayout SmartBorder a
type Refocus      a = ModifiedLayout RefocusLastLayoutHook (FocusTracking a)
type Boring       a = ModifiedLayout BoringWindows a
type ToggleFull   a = ToggleLayouts Full a
type LimitSelect  a = ModifiedLayout Selection a

type Layouts
  = Choose
      ResizableTall
      ( Choose
          (LimitSelect ResizableTall)
          (Mirror (LimitSelect ResizableTall))
      )

layoutHook' :: SmartBorders (Refocus (Boring (ToggleFull Layouts))) Window
layoutHook' =
  id
    . smartBorders
    . refocusLastLayoutHook
    . focusTracking
    . boringAuto
    . toggleLayouts Full
    -- . fullscreenFloat
    $ tall ||| limit tall ||| Mirror (limit tall)
  where
    limit :: l Window -> LimitSelect l Window
    limit = limitSelect 1 1

    tall :: ResizableTall Window
    tall = ResizableTall 1 (3/100) (1/2) []


main :: IO ()
main =
  xmonad =<< statusBar "xmobar" barPP toggleStrutsKey xconfig
  where
    xconfig =
      debugManageHookOn "M1-M4-v" $
      ewmh $
        def
          { layoutHook         = layoutHook',
            terminal           = "alacritty",
            clickJustFocuses   = False,
            normalBorderColor  = "#212121",
            focusedBorderColor = "#52626a",
            keys               = keys',
            mouseBindings      = mouseBindings',
            manageHook         = manageHook',
            handleEventHook    = eventHook,
            logHook            = logHook',
            startupHook        = startupHook'
          }

    barPP :: PP
    barPP =
      namedScratchpadFilterOutWorkspacePP $
        xmobarPP
          { ppCurrent = xmobarColor "#dddddd" "#004466" . wrap " " " ",
            ppHidden  = xmobarColor "#888888" "#222222" . wrap " " " ",
            ppSep     = " ",
            ppWsSep   = "",
            ppTitle   = const "",
            ppLayout  = \s -> if "Full" `isInfixOf` s
                                then xmobarColor "#9bd4ff" "" "·"
                                else ""
          }

    toggleStrutsKey = const (mod4Mask, xK_slash)
