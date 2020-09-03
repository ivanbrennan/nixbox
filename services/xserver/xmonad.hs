{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wall -Werror #-}

{- base -}
import Control.Monad (filterM, unless, (>=>))
import Data.Bits ((.|.))
import Data.Bool (bool)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (listToMaybe)
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
  ( Button, KeyMask, KeySym, Window, button1, controlMask, mod4Mask, noModMask, shiftMask,
    xK_1, xK_9, xK_Alt_L, xK_Alt_R, xK_Down, xK_Left, xK_Print, xK_Right, xK_Shift_R,
    xK_Tab, xK_Up, xK_a, xK_c, xK_comma, xK_d, xK_e, xK_g, xK_grave, xK_h, xK_i, xK_j,
    xK_k, xK_l, xK_m, xK_n, xK_p, xK_period, xK_q, xK_r, xK_semicolon, xK_slash, xK_space,
    xK_t, xK_u, xK_v, xK_w, xK_x, xK_z,
  )
import Graphics.X11.ExtraTypes
  ( xF86XK_AudioLowerVolume, xF86XK_AudioMute, xF86XK_AudioRaiseVolume, xF86XK_Copy,
    xF86XK_MonBrightnessDown, xF86XK_MonBrightnessUp, xF86XK_Paste,
  )
import Graphics.X11.Xlib.Extras (Event)

{- xmonad -}
import XMonad
  ( ChangeLayout (NextLayout), Choose, ExtensionClass, Full (Full), IncMasterN (IncMasterN),
    Layout, ManageHook, Mirror (Mirror), Query, Resize (Expand, Shrink), Typeable,
    WindowSet, WindowSpace, WorkspaceId, X, XConf, XConfig (XConfig), appName, className,
    clickJustFocuses, composeAll, config, doFloat, doIgnore, focus, focusedBorderColor,
    gets, handleEventHook, initialValue, io, keys, kill, layoutHook, liftX, local, logHook,
    manageHook, modMask, mouseBindings, mouseMoveWindow, normalBorderColor, refresh,
    runQuery, screenWorkspace, sendMessage, spawn, startupHook, terminal, whenJust, windows,
    windowset, withFocused, withWindowSet, workspaces, xmonad, (=?), (|||),
  )
import XMonad.StackSet
  ( RationalRect (RationalRect), allWindows, current, currentTag, findTag, focusDown',
    focusUp', hidden, integrate, shiftMaster, shiftWin, sink, stack, swapDown, swapMaster,
    swapUp, tag, view, visible, workspace,
  )
import qualified XMonad.StackSet as W

{- xmonad-contrib -}
import XMonad.Actions.CycleRecentWS (cycleWindowSets)
import XMonad.Actions.CycleWS (Direction1D (Next, Prev), WSType (WSIs), moveTo)
import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace)
import XMonad.Actions.FlexibleResize (mouseResizeEdgeWindow)
import XMonad.Actions.Submap (submap)
import XMonad.Hooks.DebugStack (debugStack)
import XMonad.Hooks.DynamicLog
  ( PP, ppCurrent, ppHidden, ppLayout, ppTitle, ppWsSep, statusBar, wrap, xmobarColor,
    xmobarPP,
  )
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.InsertPosition (Focus (Newer), Position (Above, Below), insertPosition)
import XMonad.Hooks.ManageDebug (debugManageHookOn)
import XMonad.Hooks.ManageHelpers (composeOne, doCenterFloat, doRectFloat, isDialog, (-?>))
import XMonad.Hooks.RefocusLast
  ( RefocusLastLayoutHook, isFloat, refocusLastLayoutHook, refocusLastWhen, refocusWhen,
    shiftRLWhen, swapWithLast, toggleFocus,
  )
import XMonad.Layout.BoringWindows (BoringWindows, boringWindows, focusDown, focusUp)
import XMonad.Layout.Decoration
  ( Decoration, DefaultShrinker, Theme, activeBorderColor, activeColor, activeTextColor,
    decoHeight, decoWidth, fontName, inactiveBorderColor, inactiveColor, inactiveTextColor,
    urgentBorderColor, urgentColor, urgentTextColor,
  )
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.NoBorders (SmartBorder, smartBorders)
import XMonad.Layout.ResizableTile
  ( ResizableTall (ResizableTall), MirrorResize (MirrorShrink, MirrorExpand),
  )
import XMonad.Layout.Simplest (Simplest (Simplest))
import XMonad.Layout.StateFull (FocusTracking, focusTracking)
import XMonad.Layout.SubLayouts
  ( GroupMsg (MergeAll, UnMerge, UnMergeAll), Sublayout, onGroup, pullGroup, subLayout,
  )
import XMonad.Layout.Tabbed (TabbedDecoration, addTabs, shrinkText)
import XMonad.Layout.ToggleLayouts (ToggleLayout (ToggleLayout), ToggleLayouts, toggleLayouts)
import XMonad.Layout.WindowNavigation
  ( Navigate (Go), WindowNavigation, configurableNavigation, noNavigateBorders,
  )
import XMonad.Prompt
  ( XPConfig, XPPosition (Top), alwaysHighlight, autoComplete, bgColor, fgColor, font,
    height, position, promptBorderWidth,
  )
import XMonad.Prompt.AppendFile (appendFilePrompt')
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.Prompt.Man (manPrompt)
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Prompt.Window (WindowPrompt (Goto), windowPrompt)
import qualified XMonad.Prompt.Window as P
import XMonad.Prompt.XMonad (xmonadPromptC)
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.NamedScratchpad
  ( NamedScratchpad (NS), NamedScratchpads, namedScratchpadFilterOutWorkspace,
    namedScratchpadFilterOutWorkspacePP, namedScratchpadManageHook,
  )
import qualified XMonad.Util.NamedScratchpad as NS
import XMonad.Util.Paste (sendKey)
import XMonad.Util.Types (Direction2D (D, L, R, U))


-- https://github.com/xmonad/X11/blob/6e5ef8019a0cc49e18410a335dbdeea87b7c4aac/Graphics/X11/Types.hsc
-- https://stackoverflow.com/questions/6605399/how-can-i-set-an-action-to-occur-on-a-key-release-in-xmonad

keys' :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {modMask}) =
  M.fromList $
    [ -- layout algorithms
      ( (mod4Mask, xK_space),
        sendMessage NextLayout
      ),
      ( (modMask, xK_Shift_R),
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
      ( (modMask, xK_Tab),
        cycleWindowSets recentWS [xK_Alt_L, xK_Alt_R] xK_Tab xK_grave
      ),
      -- sublayouts
      ( (mod4Mask .|. shiftMask, xK_h),
        sendMessage (pullGroup L)
      ),
      ( (mod4Mask .|. shiftMask, xK_l),
        sendMessage (pullGroup R)
      ),
      ( (mod4Mask .|. shiftMask, xK_k),
        sendMessage (pullGroup U)
      ),
      ( (mod4Mask .|. shiftMask, xK_j),
        sendMessage (pullGroup D)
      ),
      ( (mod4Mask .|. controlMask .|. shiftMask, xK_m),
        withFocused (sendMessage . MergeAll)
      ),
      ( (mod4Mask .|. controlMask .|. shiftMask, xK_u),
        withFocused (sendMessage . UnMergeAll)
      ),
      ( (mod4Mask .|. shiftMask, xK_u),
        withFocused (sendMessage . UnMerge)
      ),
      ( (mod4Mask, xK_n),
        onGroup focusDown'
      ),
      ( (mod4Mask, xK_p),
        onGroup focusUp'
      ),
      ( (modMask .|. controlMask, xK_p),
        onGroup focusUp'
      ),
      ( (modMask .|. controlMask, xK_n),
        onGroup focusDown'
      ),
      -- TODO: translate 'sendMessage (pullGroup U)' into a ManageHook.
      -- Figure out why the state toggling isn't behaving consistently.
      ( (mod4Mask, xK_i),
        mergeNext >> spawn (terminal conf) -- >> sendMessage (pullGroup U)
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
      ( (modMask, xK_semicolon),
        toggleFocus
      ),
      ( (mod4Mask, xK_semicolon),
        swapWithLast
      ),
      -- swap
      ( (modMask .|. shiftMask, xK_m),
        windows swapMaster
      ),
      ( (modMask .|. shiftMask, xK_j),
        windows swapDown -- TODO: swap subgroups
      ),
      ( (modMask .|. shiftMask, xK_k),
        windows swapUp -- TODO: swap subgroups
      ),
      -- resize
      ( (modMask .|. controlMask, xK_h),
        sendMessage Shrink
      ),
      ( (modMask .|. controlMask, xK_l),
        sendMessage Expand
      ),
      ( (modMask .|. controlMask, xK_j),
        sendMessage MirrorShrink
      ),
      ( (modMask .|. controlMask, xK_k),
        sendMessage MirrorExpand
      ),
      -- increment/decrement master area
      ( (modMask .|. shiftMask, xK_comma),
        sendMessage (IncMasterN 1)
      ),
      ( (modMask .|. shiftMask, xK_period),
        sendMessage (IncMasterN (-1))
      ),
      -- refresh
      ( (mod4Mask .|. shiftMask, xK_r),
        refresh
      ),
      -- tile
      ( (mod4Mask, xK_t),
        withFocused (windows . sink)
      ),
      -- quit or restart
      ( (mod4Mask, xK_q),
        spawn "xmonad --recompile && xmonad --restart"
      ),
      ( (mod4Mask .|. shiftMask, xK_q),
        confirmPrompt xPConfig "exit" (io exitSuccess)
      ),
      -- launch/kill
      ( (modMask, xK_i),
        spawn (terminal conf)
      ),
      ( (modMask, xK_space),
        spawn "dmenu_run -fn monospace:size=12 -l 24 -i -nb '#1c1c1c' -nf '#a5adb7' -sb '#222222' -sf '#ffffff'"
      ),
      ( (controlMask, xK_Shift_R),
        namedScratchpadAction scratchpads (NS.name scratchTerminal)
      ),
      ( (mod4Mask .|. modMask, xK_p),
        debugStack
      ),
      ( (modMask, xK_slash),
        submap . M.fromList $
          [ ( (modMask, xK_slash),
              local (setTerminal "alacritty --class=manpage") $
                manPrompt (xPConfig {autoComplete = Just 0})
            )
          ]
      ),
      ( (controlMask, xK_space),
        submap . M.fromList $
          [ ( (noModMask, xK_a),
              appendThoughtPrompt xPConfig
            ),
            ( (noModMask, xK_g),
              windowPrompt xPConfig Goto P.allWindows
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
  where
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    workspaceTagKeys =
      [ ( (m .|. modMask, k),
          windows =<< f i
        )
        | (k, i) <- zip [xK_1..xK_9] (workspaces conf),
          (m, f) <- [ (noModMask, pure . view),
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
          (f, m) <- [ (pure . view, noModMask),
                      (shiftRLWhen isFloat, shiftMask)
                    ]
      ]

    cycledWorkspace :: WSType
    cycledWorkspace = WSIs $ pure (not . boringWorkspace)

    boringWorkspace :: WindowSpace -> Bool
    boringWorkspace ws = emptyWorkspace ws || scratchpadWorkspace ws

    emptyWorkspace :: WindowSpace -> Bool
    emptyWorkspace = null . stack

    scratchpadWorkspace :: WindowSpace -> Bool
    scratchpadWorkspace = null . namedScratchpadFilterOutWorkspace . (:[])

    toggleRecentWS :: X ()
    toggleRecentWS =
      gets (recentWS . windowset) >>= windows . const . head

    recentWS :: WindowSet -> [WindowSet]
    recentWS ws = map (`view` ws) (recentTags ws)

    recentTags :: WindowSet -> [WorkspaceId]
    recentTags ws =
      map tag
        . filter (not . null . stack)
        . namedScratchpadFilterOutWorkspace
        $ map workspace (visible ws)
          ++ hidden ws
          ++ [workspace (current ws)]

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

    commands :: [(String, X ())]
    commands =
      [ ("shrink", sendMessage Shrink),
        ("expand", sendMessage Expand),
        ("refresh", refresh)
      ]


-- Reimplement to apply RefocusLast when removing a scratchpad from a workspace.
namedScratchpadAction :: NamedScratchpads -- ^ Named scratchpads configuration
                      -> String           -- ^ Scratchpad name
                      -> X ()
namedScratchpadAction = customRunNamedScratchpadAction runApplication

-- | Action to pop up specified named scratchpad, given a custom way to initially start the application.
customRunNamedScratchpadAction :: (NamedScratchpad -> X ())  -- ^ Function initially running the application, given the configured @scratchpad@ cmd
                               -> NamedScratchpads           -- ^ Named scratchpads configuration
                               -> String                     -- ^ Scratchpad name
                               -> X ()
customRunNamedScratchpadAction = someNamedScratchpadAction (\f ws -> f $ NE.head ws)

-- | execute some action on a named scratchpad
someNamedScratchpadAction :: ((Window -> X ()) -> NonEmpty Window -> X ())
                          -> (NamedScratchpad -> X ())
                          -> NamedScratchpads
                          -> String
                          -> X ()
someNamedScratchpadAction f runApp scratchpadConfig scratchpadName =
    someNamedScratchpadAction' f launchTo dismissTo runApp scratchpadConfig scratchpadName
    where
        launchTo :: WorkspaceId -> Window -> X ()
        launchTo i = (shiftWinRLWhen isFloat i >=> windows)

        dismissTo :: WorkspaceId -> Window -> X ()
        dismissTo i = const (shiftRLWhen isFloat i >>= windows)

someNamedScratchpadAction' :: ((Window -> X ()) -> NonEmpty Window -> X ())
                           -> (WorkspaceId -> Window -> X ())
                           -> (WorkspaceId -> Window -> X ())
                           -> (NamedScratchpad -> X ())
                           -> NamedScratchpads
                           -> String
                           -> X ()
someNamedScratchpadAction' f launchTo dismissTo runApp scratchpadConfig scratchpadName =
    case findByName scratchpadConfig scratchpadName of
        Nothing -> return ()

        Just conf -> withWindowSet $ \winSet -> do
            let focusedWspWindows = maybe [] integrate (stack . workspace . current $ winSet)
            matchingOnCurrent <- filterM (runQuery (NS.query conf)) focusedWspWindows

            case nonEmpty matchingOnCurrent of
                -- no matching window on the current workspace -> scratchpad not running or in background
                Nothing -> launch conf winSet
                -- matching window running on current workspace -> window should be shifted to scratchpad workspace
                Just wins -> dismiss winSet wins
    where
        launch conf winSet = do
            matchingOnAll <- filterM (runQuery (NS.query conf)) (allWindows winSet)
            case nonEmpty matchingOnAll of
                Nothing   -> runApp conf
                Just wins -> f (launchTo (currentTag winSet)) wins

        dismiss winSet wins = do
            unless (any (\wsp -> scratchpadWorkspaceTag == tag wsp) (W.workspaces winSet))
                (addHiddenWorkspace scratchpadWorkspaceTag)
            f (dismissTo scratchpadWorkspaceTag) wins

-- | Runs application which should appear in specified scratchpad
runApplication :: NamedScratchpad -> X ()
runApplication = spawn . NS.cmd

-- | Finds named scratchpad configuration by name
findByName :: NamedScratchpads -> String -> Maybe NamedScratchpad
findByName c s = listToMaybe $ filter ((s ==) . NS.name) c

-- tag of the scratchpad workspace
scratchpadWorkspaceTag :: String
scratchpadWorkspaceTag = "NSP"

shiftWinRLWhen :: Query Bool -> WorkspaceId -> Window -> X (WindowSet -> WindowSet)
shiftWinRLWhen p to w = withWindowSet $ \ws ->
  case findTag w ws of
    Just from -> do
      refocus <- refocusWhen p from
      let shift' = shiftWin to w
      pure (refocus . shift')
    _ -> pure id


mouseBindings' :: XConfig Layout -> Map (KeyMask, Button) (Window -> X ())
mouseBindings' XConfig {modMask} =
  M.fromList $
    [ ( (modMask, button1),
        \w ->
          focus w
            >> mouseMoveWindow w
            >> windows shiftMaster
      ),
      ( (mod4Mask, button1),
        \w ->
          focus w
            >> mouseResizeEdgeWindow (3/4) w
            >> windows shiftMaster
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
  doRectFloat (RationalRect x y width height)
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
        [ -- TODO: replace 'insertPosition Below Newer' with a ManageHook
          -- that merges the new window into a sublayout group.
          isMerging -?> insertPosition Below Newer,
          isDialog -?> doFloat,
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
  where
    isMerging :: Query Bool
    isMerging = (liftX . XS.modified) $ const (MergeNext False)

newtype MergeNext = MergeNext Bool deriving (Eq, Typeable)

instance ExtensionClass MergeNext where
  initialValue  = MergeNext False

mergeNext :: X ()
mergeNext = XS.put (MergeNext True)

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


layout ::
  ModifiedLayout
    WindowNavigation
    ( ModifiedLayout
        SmartBorder
        ( ModifiedLayout
            RefocusLastLayoutHook
            ( FocusTracking
                ( ModifiedLayout
                    (Decoration TabbedDecoration DefaultShrinker)
                    ( ModifiedLayout
                        (Sublayout Simplest)
                        ( ModifiedLayout
                            BoringWindows
                            ( ToggleLayouts
                                Full
                                (Choose ResizableTall (Mirror ResizableTall))
                            )
                        )
                    )
                )
            )
        )
    )
    Window
layout =
  id
    . configurableNavigation noNavigateBorders
    . smartBorders
    . refocusLastLayoutHook
    . focusTracking
    . addTabs shrinkText theme
    . subLayout [] Simplest
    . boringWindows
    . toggleLayouts Full
    -- . fullscreenFloat
    $ tiled ||| Mirror tiled
  where
    theme :: Theme
    theme =
      def
        { activeColor         = "#1f1f1f",
          inactiveColor       = "#1f1f1f",
          urgentColor         = "#15539e",
          activeBorderColor   = "#161616",
          inactiveBorderColor = "#1f1f1f",
          urgentBorderColor   = "#030c17",
          activeTextColor     = "#d3d3d3",
          inactiveTextColor   = "#757d80",
          urgentTextColor     = "#ffffff",
          fontName            = "xft:Cantarell:bold:size=10",
          decoWidth           = 200,
          decoHeight          = 20
        }

    tiled :: ResizableTall a
    tiled = ResizableTall 1 (3/100) (1/2) []


main :: IO ()
main =
  xmonad =<< statusBar "xmobar" barPP toggleStrutsKey xconfig
  where
    xconfig =
      debugManageHookOn "M1-M4-v" $
      ewmh $
        def
          { layoutHook         = layout,
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
            ppWsSep   = "",
            ppTitle   = const "",
            ppLayout  = const ""
          }

    toggleStrutsKey = const (mod4Mask, xK_slash)
