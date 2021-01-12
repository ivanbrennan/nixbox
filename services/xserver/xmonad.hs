{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wall -Werror -O2 #-}

{- base -}
import Control.Arrow (second)
import Control.Monad (when, (>=>))
import Data.Bits ((.|.))
import Data.Dynamic (Typeable)
import Data.List (intercalate, isInfixOf)
import Data.Monoid (All)
import System.Directory (getHomeDirectory)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.FilePath ((</>))
import System.IO (Handle)
import System.Info (arch, os)
import Text.Printf (printf)

{- containers -}
import Data.Map (Map)
import qualified Data.Map as M

{- data-default -}
import Data.Default (def)

{- process -}
import System.Process.Internals (translate)

{- unix -}
import System.Posix.Process (executeFile)

{- X11 -}
import Graphics.X11
  ( Button, KeyMask, KeySym, Window, button1, controlMask, mod1Mask, mod4Mask,
    noModMask, shiftMask, xK_1, xK_9, xK_Alt_L, xK_Alt_R, xK_BackSpace, xK_Delete,
    xK_Insert, xK_Print, xK_Tab, xK_a, xK_c, xK_comma, xK_d, xK_e, xK_f, xK_g,
    xK_h, xK_i, xK_j, xK_k, xK_l, xK_m, xK_n, xK_p, xK_period, xK_q, xK_r,
    xK_semicolon, xK_slash, xK_space, xK_t, xK_u, xK_v, xK_w, xK_x, xK_y, xK_z,
  )
import Graphics.X11.ExtraTypes
  ( xF86XK_AudioLowerVolume, xF86XK_AudioMute, xF86XK_AudioRaiseVolume,
    xF86XK_Copy, xF86XK_MonBrightnessDown, xF86XK_MonBrightnessUp, xF86XK_Paste,
  )
import Graphics.X11.Xlib.Extras (Event)

{- xmonad -}
import XMonad
  ( ChangeLayout (NextLayout), Choose, ExtensionClass, IncMasterN (IncMasterN),
    Layout, ManageHook, MonadIO, Query, Resize (Expand, Shrink), ScreenId (S),
    StateExtension (PersistentExtension), WindowSet, WindowSpace, WorkspaceId, X,
    XConf, XConfig (XConfig), appName, catchIO, className, clickJustFocuses,
    composeAll, config, description, doFloat, doIgnore, extensionType, focus,
    focusedBorderColor, getXMonadDataDir, handleEventHook, initialValue, io, keys,
    kill, launch, layoutHook, local, logHook, manageHook, modMask, mouseBindings,
    mouseMoveWindow, normalBorderColor, recompile, refresh, restart,
    screenWorkspace, sendMessage, spawn, startupHook, terminal, trace, whenJust,
    whenX, windows, withFocused, withWindowSet, workspaces, writeStateToFile,
    (=?), (|||),
  )
import qualified XMonad.StackSet as W

{- xmonad-contrib -}
import XMonad.Actions.CycleWS
  ( Direction1D (Next, Prev), WSType (WSIs, NonEmptyWS), moveTo,
  )
import XMonad.Actions.DynamicProjects (dynamicProjects, changeProjectDirPrompt)
import XMonad.Actions.FlexibleResize (mouseResizeEdgeWindow)
import XMonad.Actions.PerWindowKeys (bindFirst)
import XMonad.Actions.RotateSome (surfaceNext, surfacePrev)
import XMonad.Actions.RotSlaves (rotAllDown, rotAllUp, rotSlavesDown, rotSlavesUp)
import XMonad.Actions.Submap (submap)
import XMonad.Actions.WindowBringer (gotoMenuArgs)
import XMonad.Actions.WorkspaceNames (getWorkspaceNames', renameWorkspace)
import XMonad.Hooks.DebugStack (debugStackString)
import XMonad.Hooks.DynamicBars
  ( dynStatusBarEventHook, dynStatusBarStartup, multiPPFormat,
  )
import XMonad.Hooks.DynamicLog
  ( PP, dynamicLogString, pad, ppCurrent, ppHidden, ppHiddenNoWindows, ppLayout,
    ppSep, ppTitle, ppUrgent, ppVisible, ppWsSep, wrap, xmobarColor, xmobarPP,
  )
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.InsertPosition
  ( Focus (Newer), Position (Above), insertPosition,
  )
import XMonad.Hooks.ManageDebug (debugManageHookOn)
import XMonad.Hooks.ManageDocks (ToggleStruts (ToggleStruts), avoidStruts, docks)
import XMonad.Hooks.ManageHelpers
  ( composeOne, doCenterFloat, doRectFloat, isDialog, (-?>),
  )
import XMonad.Hooks.RefocusLast
  ( RefocusLastLayoutHook, isFloat, refocusLastLayoutHook, refocusLastWhen,
    shiftRLWhen, swapWithLast, toggleFocus,
  )
import XMonad.Layout.BoringWindows
  ( BoringWindows, boringAuto, focusDown, focusUp, siftDown, siftUp,
  )
import XMonad.Layout.IndependentScreens
  ( PhysicalWorkspace, VirtualWorkspace, countScreens, marshall, marshallPP,
    unmarshallS, withScreens, workspaces',
  )
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.LimitWindows
  ( Selection, limitSelect, decreaseLimit, increaseLimit,
  )
import XMonad.Layout.MultiToggle
  ( EOT, HCons, MultiToggle, Toggle (Toggle), mkToggle1,
  )
import XMonad.Layout.MultiToggle.Instances (StdTransformers (FULL, MIRROR))
import XMonad.Layout.NoBorders (SmartBorder, smartBorders)
import XMonad.Layout.Renamed (Rename (Append, Prepend), renamed)
import XMonad.Layout.ResizableTile
  ( ResizableTall (ResizableTall), MirrorResize (MirrorShrink, MirrorExpand),
  )
import XMonad.Layout.StateFull (FocusTracking, focusTracking)
import XMonad.Prompt
  ( ComplCaseSensitivity (ComplCaseSensitive), XPConfig, XPPosition (Top),
    alwaysHighlight, bgColor, bgHLight, complCaseSensitivity, defaultXPKeymap,
    fgColor, fgHLight, font, height, moveHistory, position, promptBorderWidth,
    promptKeymap,
  )
import XMonad.Prompt.AppendFile (appendFilePrompt')
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.Prompt.Man (manPrompt)
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Prompt.Workspace (workspacePrompt)
import XMonad.Prompt.XMonad (xmonadPromptC)
import XMonad.Util.ClickableWorkspaces (clickableWrap)
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Loggers (date)
import XMonad.Util.NamedScratchpad
  ( NamedScratchpad (NS), namedScratchpadAction,
    namedScratchpadFilterOutWorkspace, namedScratchpadFilterOutWorkspacePP,
    namedScratchpadManageHook,
  )
import qualified XMonad.Util.NamedScratchpad as NS
import XMonad.Util.Paste (sendKey)
import XMonad.Util.Run (safeSpawn, safeSpawnProg, spawnPipe)
import XMonad.Util.WorkspaceCompare (getWsIndex)


main :: IO ()
main =
  launch . xconfig =<< countScreens
  where
    xconfig nScreens =
      docks $
      debugManageHookOn "M1-M4-v" $
      dynamicProjects [] $
      ewmh $
        def
          { layoutHook         = avoidStruts layoutHook',
            workspaces         = withScreens nScreens (workspaces def),
            startupHook        = startupHook',
            logHook            = logHook',
            handleEventHook    = handleEventHook',
            manageHook         = manageHook',
            keys               = keys',
            mouseBindings      = mouseBindings',
            terminal           = "alacritty",
            clickJustFocuses   = False,
            normalBorderColor  = grey1,
            focusedBorderColor = grey2
          }


type SmartBorders a = ModifiedLayout SmartBorder a
type Refocus      a = ModifiedLayout RefocusLastLayoutHook (FocusTracking a)
type Boring       a = ModifiedLayout BoringWindows a
type Renamed      a = ModifiedLayout Rename a
type LimitSelect  a = ModifiedLayout Selection a
type ToggleFull   a = MultiToggle (HCons StdTransformers EOT) a
type ToggleMirror a = MultiToggle (HCons StdTransformers EOT) a

type Layouts
  = ToggleFull
      ( ToggleMirror
        ( Choose
            ResizableTall
            (Renamed (LimitSelect ResizableTall))
        )
      )

layoutHook' :: SmartBorders (Refocus (Boring Layouts)) Window
layoutHook' =
  id
    . smartBorders
    . refocusLastLayoutHook
    . focusTracking
    . boringAuto
    . mkToggle1 FULL
    . mkToggle1 MIRROR
    $ tall ||| rename (limit tall)
  where
    limit :: l Window -> LimitSelect l Window
    limit = limitSelect 1 1

    rename :: l Window -> Renamed l Window
    rename = renamed [Prepend "Limit (", Append ")"]

    tall :: ResizableTall Window
    tall = ResizableTall 1 (1/100) (52/100) []


startupHook' :: X ()
startupHook' = do
  systray
  io killAlsactl
  dynStatusBarStartup xmobar killAlsactl


systray :: X ()
systray = spawn $
  intercalate " ; " [killTray, execTray]
  where
    killTray =
      intercalate
        " | "
        [ "ps --no-headers -o pid,command -C trayer",
          "awk '/\\<trayer .*SetDockType true\\>/ { print $1 }'",
          "xargs --no-run-if-empty kill 2>/dev/null"
        ]

    execTray =
      unwords $
        "exec" :
        [ "trayer",
          "--edge top",
          "--align right",
          "--height 22",
          "--widthtype request",
          "--expand true",
          "--SetDockType true",
          "--SetPartialStrut true",
          "--transparent true",
          "--alpha 0",
          "--tint 0x161616",
          "--monitor 0"
        ]

-- https://github.com/jaor/xmobar/issues/432
killAlsactl :: MonadIO m => m ()
killAlsactl = spawn $
  intercalate
    " | "
    [ "ps --no-headers -o pid,command -C alsactl",
      "awk '/alsactl monitor default$/ { print $1 }'",
      "xargs --no-run-if-empty kill 2>/dev/null"
    ]


xmobar :: ScreenId -> IO Handle
xmobar s@(S i) = spawnPipe $
  unwords
    [ "xmobar",
      "-B", translate black,
      "-F", translate grey6,
      "-f", "xft:monospace:size=11",
      "-N", "xft:FontAwesome:size=11",
      "-i", "/run/current-system/sw/share/icons/xmobar",
      "-x", show i,
      "-p", translate "Top",
      "-t", translate (xmobarTemplate s),
      "-c", translate $ list (xmobarCommands s)
    ]

xmobarTemplate :: ScreenId -> String
xmobarTemplate (S i) = concat $
  if i == 0
    then
      [ cmd "UnsafeStdinReader",
        pad "}{",
        cmd "disku",
        " ",
        xmobarColor cyan "" "·",
        cmd "cpu",
        "  ",
        fontN 1 $ xmobarColor grey2 "" (cmd "vpn"),
        pad (cmd "battery"),
        pad (cmd "alsa:default:Master"),
        pad (cmd "date"),
        pad (cmd "trayerpad")
      ]
    else
      [ cmd "UnsafeStdinReader",
        pad "}{"
      ]
  where
    cmd = wrap "%" "%"

xmobarCommands :: ScreenId -> [String]
xmobarCommands (S i) = map unwords $
  if i == 0
    then [disk, cpu, vpn, battery, volume, date', unsafeStdinReader, trayerpad]
    else [unsafeStdinReader]
  where
    disk =
      [ "Run DiskU",
        brackets $ show ("/", "<free>"),
        list (map quote diskArgs),
        "50"
      ]
    diskArgs =
      [ "--Low"   , "5",
        "--high"  , grey3,
        "--normal", grey3,
        "--low"   , red
      ]

    cpu = ["Run Cpu", list (map quote cpuArgs), "50"]
    cpuArgs =
      [ "--template", "<total>",
        "--ppad"    , "2",
        "--High"    , "50",
        "--Low"     , "3",
        "--high"    , orange,
        "--normal"  , grey2,
        "--low"     , grey2
      ]

    vpn = ["Run Com", quote "bleep", list [], quote "vpn", "50"]

    battery = ["Run Battery", list (map quote batteryArgs), "20"]
    batteryArgs =
      [ "--template", "<acstatus>",
        "--",
        "--on-icon-pattern"  , icon "battery/on/battery_on_%%.xpm",
        "--idle-icon-pattern", icon "battery/idle/battery_idle_%%.xpm",
        "--off-icon-pattern" , icon "battery/off/battery_off_%%.xpm",
        "--on"               , "<leftipat>",
        "--idle"             , "<leftipat>",
        "--off"              , "<leftipat>"
      ]

    volume =
      ["Run Alsa", quote "default", quote "Master", list (map quote volumeArgs)]
    volumeArgs =
      [ "--template", "<status>",
        "--",
        "--on"  , fontN 1 "\xf026" ++ "<volume>",
        "--off" , fontN 1 "\xf026" ++ "<volume>",
        "--onc" , grey6,
        "--offc", grey2
      ]

    date' = ["Run Date", quote dateFormat, quote "date", "50"]
    dateFormat = "%a %b %-d  " ++ xmobarColor chalk "" "%l:%M"

    unsafeStdinReader = ["Run UnsafeStdinReader"]

    trayerpad =
      ["Run Com", quote "trayer-padding-icon", list [], quote "trayerpad", "10"]

list :: [String] -> String
list = brackets . intercalate ","

brackets :: String -> String
brackets s = "[" ++ s ++ "]"

quote :: String -> String
quote = wrap "\"" "\""

icon :: String -> String
icon = wrap "<icon=" "/>"

fontN :: Int -> String -> String
fontN n = wrap ("<fn=" ++ show n ++ ">") "</fn>"


logHook' :: X ()
logHook' = multiPP currentScreenPP nonCurrentScreenPP
  where
    multiPP :: PP -> PP -> X ()
    multiPP = multiPPFormat (withCurrentScreen . logString)

    logString :: PP -> ScreenId -> X String
    logString pp =
      composePP pp >=> dynamicLogString

    composePP :: PP -> ScreenId -> X PP
    composePP pp s = do
      names <- getWorkspaceNames (marshall s)
      click <- getClickable (marshall s . omitName)
      pure
        . namedScratchpadFilterOutWorkspacePP
        . marshallPP s
        $ pp
          { ppCurrent         = ppCurrent         pp . click . names,
            ppVisible         = ppVisible         pp . click . names,
            ppHidden          = ppHidden          pp . click . names,
            ppHiddenNoWindows = ppHiddenNoWindows pp . click . names,
            ppUrgent          = ppUrgent          pp . click . names
          }

    currentScreenPP :: PP
    currentScreenPP = barPP

    nonCurrentScreenPP :: PP
    nonCurrentScreenPP = barPP

    barPP :: PP
    barPP =
      xmobarPP
        { ppCurrent = xmobarColor grey7 blue . pad,
          ppHidden  = xmobarColor grey4 grey1 . pad,
          ppSep     = " ",
          ppWsSep   = "",
          ppTitle   = const "",
          ppLayout  = ppLayout'
        }

    ppLayout' :: String -> String
    ppLayout' s
      | "Full"  `isInfixOf` s = xmobarColor cyan "" "·"
      | "Limit" `isInfixOf` s = xmobarColor grey2 "" "·"
      | otherwise             = ""

    omitName :: WorkspaceId -> WorkspaceId
    omitName = takeWhile (/= ':')

    getWorkspaceNames :: (WorkspaceId -> WorkspaceId) -> X (WorkspaceId -> String)
    getWorkspaceNames f = do
      name <- getWorkspaceNames'
      pure $ \wks -> wks ++ maybe "" (':' :) (name $ f wks)

    getClickable :: (WorkspaceId -> WorkspaceId) -> X (VirtualWorkspace -> String)
    getClickable f = do
      wsIndex <- getWsIndex
      pure $ \wks -> maybe wks (`clickableWrap` wks) (wsIndex $ f wks)

withCurrentScreen :: (ScreenId -> X a) -> X a
withCurrentScreen f =
  withWindowSet (f . W.screen . W.current)


handleEventHook' :: Event -> X All
handleEventHook' =
  dynStatusBarEventHook xmobar killAlsactl
    <> refocusLastWhen isFloat
    <> fullscreenEventHook


manageHook' :: ManageHook
manageHook' =
  composeAll
    [ composeOne
        [ appName =? "desktop_window" -?> doIgnore,
          appName =? "manpage" -?> doCenteredFloat 0.6 0.6,
          isDialog -?> doFloat,
          className =? "Gcr-prompter" -?> doCenterFloat,
          className =? "Xmessage" -?> doCenterFloat
        ],
      namedScratchpadManageHook scratchpads,
      insertPosition Above Newer
    ]

doCenteredFloat :: Rational -> Rational -> ManageHook
doCenteredFloat width height =
  doRectFloat (W.RationalRect x y width height)
  where
    x :: Rational
    x = (1 - width) / 2

    y :: Rational
    y = (1 - height) / 2

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


-- https://github.com/xmonad/X11/blob/6e5ef8019a0cc49e18410a335dbdeea87b7c4aac/Graphics/X11/Types.hsc
-- https://stackoverflow.com/questions/6605399/how-can-i-set-an-action-to-occur-on-a-key-release-in-xmonad

keys' :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {modMask}) =
  M.fromList $
    [ -- layout algorithms
      ( (mod4Mask, xK_space),
        fullToggleOff >> sendMessage NextLayout
      ),
      ( (mod4Mask .|. shiftMask, xK_space),
        fullToggleOff >> sendMessage (Toggle MIRROR)
      ),
      ( (modMask, xK_space),
        sendMessage (Toggle FULL)
      ),
      ( (mod4Mask, xK_slash),
        sendMessage ToggleStruts
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
      ( (modMask .|. shiftMask, xK_semicolon),
        changeProjectDirPrompt xPConfig
      ),
      -- focus
      ( (modMask, xK_j),
        do
          full <- isFullToggleOn
          if full then windows W.focusDown else focusDown
      ),
      ( (modMask, xK_k),
        do
          full <- isFullToggleOn
          if full then windows W.focusUp else focusUp
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
      ( (mod4Mask .|. controlMask, xK_k),
        rotAllDown
      ),
      ( (mod4Mask .|. controlMask, xK_j),
        rotAllUp
      ),
      ( (modMask .|. controlMask, xK_n),
        surfaceNext
      ),
      ( (modMask .|. controlMask, xK_p),
        surfacePrev
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
        siftDown
      ),
      ( (modMask .|. shiftMask, xK_k),
        siftUp
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
      ( (mod4Mask .|. controlMask, xK_r),
        refresh
      ),
      -- tile
      ( (mod4Mask, xK_t),
        withFocused (windows . W.sink)
      ),
      -- exec the xmonad found on PATH, resuming state
      ( (mod4Mask, xK_q),
        restart "xmonad" True
      ),
      -- exec the xmonad found on PATH, discarding state
      ( (mod4Mask .|. controlMask, xK_q),
        restart "xmonad" False
      ),
      -- compile ~/.xmonad/xmonad.hs and exec the result, resuming state
      ( (mod4Mask .|. mod1Mask, xK_q),
        compileRestart True
      ),
      -- compile ~/.xmonad/xmonad.hs and exec the result, discarding state
      ( (mod4Mask .|. mod1Mask .|. controlMask, xK_q),
        compileRestart False
      ),
      -- quit xmonad
      ( (mod4Mask .|. shiftMask, xK_q),
        confirmPrompt xPConfig "exit" (io exitSuccess)
      ),
      -- launch/kill
      ( (modMask, xK_i),
        safeSpawnProg (terminal conf)
      ),
      ( (controlMask, xK_space),
        safeSpawn "dmenu_run" dmenuOpts
      ),
      ( (controlMask .|. shiftMask, xK_space),
        namedScratchpadAction scratchpads (NS.name scratchTerminal)
      ),
      ( (mod4Mask .|. modMask, xK_y),
        debugStackString
          >>= trace . unlines . uncurry (++) . second reverse . splitAt 1 . lines
      ),
      ( (noModMask, xK_Print),
        safeSpawn "screenshot" []
      ),
      ( (controlMask, xK_Print),
        safeSpawn "screenshot" ["-c"]
      ),
      ( (shiftMask, xK_Print),
        safeSpawn "screenshot" ["-a"]
      ),
      ( (controlMask .|. shiftMask, xK_Print),
        safeSpawn "screenshot" ["-a", "-c"]
      ),
      ( (mod4Mask, xK_Print),
        screencast []
      ),
      ( (mod4Mask .|. shiftMask, xK_Print),
        screencast ["-a"]
      ),
      ( (mod4Mask .|. shiftMask, xK_d),
        kill
      ),
      ( (modMask .|. shiftMask, xK_space),
        safeSpawn "passmenu" dmenuOpts
      ),
      -- volume
      ( (noModMask, xF86XK_AudioRaiseVolume),
        spawn "pactl set-sink-mute @DEFAULT_SINK@ 0 && pactl set-sink-volume @DEFAULT_SINK@ +2%"
      ),
      ( (noModMask, xF86XK_AudioLowerVolume),
        safeSpawn "pactl" ["set-sink-volume", "@DEFAULT_SINK@", "-2%"]
      ),
      ( (noModMask, xF86XK_AudioMute),
        safeSpawn "pactl" ["set-sink-mute", "@DEFAULT_SINK@", "toggle"]
      ),
      ( (mod4Mask, xF86XK_AudioMute),
        safeSpawn "pactl" ["set-source-mute", "@DEFAULT_SOURCE@", "toggle"]
      ),
      ( (mod4Mask, xF86XK_AudioRaiseVolume),
        spawn "pactl set-source-mute @DEFAULT_SOURCE@ 0 && pactl set-source-volume @DEFAULT_SOURCE@ +2%"
      ),
      ( (mod4Mask, xF86XK_AudioLowerVolume),
        safeSpawn "pactl" ["set-source-volume", "@DEFAULT_SOURCE@", "-2%"]
      ),
      -- brightness
      ( (noModMask, xF86XK_MonBrightnessUp),
        safeSpawn "light" ["-A", "20"]
      ),
      ( (noModMask, xF86XK_MonBrightnessDown),
        safeSpawn "light" ["-U", "20"]
      ),
      -- copy/paste
      ( (modMask, xK_c),
        ifTerminal
          (sendKey noModMask xF86XK_Copy)
          (sendKey controlMask xK_c)
      ),
      ( (modMask, xK_v),
        ifTerminal
          (sendKey noModMask xF86XK_Paste)
          (sendKey controlMask xK_v)
      )
    ]
      ++ workspaceTagKeys
      ++ screenKeys
      ++ [ ( (modMask, xK_slash),
             submap . M.fromList $
               [ ( (modMask, xK_slash),
                   local
                     (setTerminal "alacritty --class=manpage")
                     (manPrompt xPConfig)
                 )
               ]
           )
         ]
      ++ [ ( (modMask, alt),
             submap . M.fromList $
               -- [ ( (noModMask, xK_u),
               --     switchProjectPrompt xPConfig
               --   ),
               --   ( (shiftMask, xK_u),
               --     shiftToProjectPrompt xPConfig
               --   ),
               --   ( (noModMask, xK_m),
               --     renameProjectPrompt xPConfig
               --   ),
               --   ( (noModMask, xK_c),
               --     changeProjectDirPrompt xPConfig
               --   ),
               [ ( (noModMask, xK_a),
                   appendThoughtPrompt xPConfig
                 ),
                 ( (noModMask, xK_f),
                   gotoMenuArgs dmenuOpts
                 ),
                 ( (noModMask, xK_g),
                   dmenuSpawnTerminal
                 ),
                 ( (noModMask, xK_m),
                   renameWorkspace xPConfig
                 ),
                 ( (noModMask, xK_n),
                   safeSpawnProg "networkmanager_dmenu"
                 ),
                 ( (noModMask, xK_Insert),
                   safeSpawn "udisks_dmenu" ("mount" : dmenuOpts)
                 ),
                 ( (noModMask, xK_Delete),
                   safeSpawn "udisks_dmenu" ("unmount" : dmenuOpts)
                 ),
                 ( (noModMask, xK_v),
                   safeSpawn "openvpn_dmenu" dmenuOpts
                 ),
                 ( (shiftMask, xK_v),
                   safeSpawn "openvpn_dmenu" ("restart" : dmenuOpts)
                 ),
                 ( (noModMask, xK_r),
                   runOrRaisePrompt xPConfig
                 ),
                 ( (noModMask, xK_w),
                   workspacePrompt xPConfig (windows . W.view)
                 ),
                 ( (noModMask, xK_x),
                   xmonadPromptC commands xPConfig
                 ),
                 ( (noModMask, xK_y),
                   layoutDescription >>= trace
                 ),
                 ( (noModMask, xK_z),
                   safeSpawn "i3lock" ["--color", grey0]
                 ),
                 ( (noModMask, xK_u),
                   safeSpawn "dunstctl" ["close-all"]
                 ),
                 ( (noModMask, xK_BackSpace),
                   safeSpawn "dunstctl" ["history-pop"]
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
          windows =<< onCurrentScreenX f i
        )
        | (k, i) <- zip [xK_1..xK_9] (workspaces' conf),
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

    onCurrentScreenX :: (PhysicalWorkspace -> X a) -> (VirtualWorkspace -> X a)
    onCurrentScreenX f vwsp =
      withCurrentScreen (f . flip marshall vwsp)

    compileRestart :: Bool -> X ()
    compileRestart resume =
      whenX (recompile True) $
        when resume writeStateToFile
          *> catchIO
            ( do
              dir <- getXMonadDataDir
              args <- getArgs
              executeFile (dir </> printf "xmonad-%s-%s" arch os) False args Nothing
            )

    fullToggleOff :: X ()
    fullToggleOff = do
      on <- isFullToggleOn
      when on $ sendMessage (Toggle FULL)

    isFullToggleOn :: X Bool
    isFullToggleOn =
      fmap ("Full" `isInfixOf`) layoutDescription

    layoutDescription :: X String
    layoutDescription = withWindowSet $
      pure . description . W.layout . W.workspace . W.current

    cycledWorkspace :: WSType
    cycledWorkspace =
      WSIs $ withCurrentScreen (pure . cycledWorkspaceOnScreen)

    cycledWorkspaceOnScreen :: ScreenId -> WindowSpace -> Bool
    cycledWorkspaceOnScreen s wsp =
      all
        ($ wsp)
        [ (not . scratchpadWorkspace),
          (not . emptyWorkspace),
          isOnScreen s
        ]

    emptyWorkspace :: WindowSpace -> Bool
    emptyWorkspace = null . W.stack

    scratchpadWorkspace :: WindowSpace -> Bool
    scratchpadWorkspace = null . namedScratchpadFilterOutWorkspace . (:[])

    isOnScreen :: ScreenId -> WindowSpace -> Bool
    isOnScreen s = (s ==) . unmarshallS . W.tag

    toggleRecentWS :: X ()
    toggleRecentWS =
      withWindowSet (windows . const . head . recentWS)

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

    setTerminal :: String -> XConf -> XConf
    setTerminal t xc =
      xc {config = (config xc) {terminal = t}}

    ifTerminal :: X () -> X () -> X ()
    ifTerminal thenX elseX =
      bindFirst [(isTerminal, thenX), (pure True, elseX)]

    isTerminal :: Query Bool
    isTerminal = className =? "Alacritty"

    appendThoughtPrompt :: XPConfig -> X ()
    appendThoughtPrompt xP = do
      home <- io getHomeDirectory
      time <- maybe "" id <$> date "[%Y-%m-%dT%T%Z] "
      appendFilePrompt' xP (time ++) (home </> "thoughts")

    dmenuSpawnTerminal :: X ()
    dmenuSpawnTerminal =
      spawn $
        intercalate
          " | "
          [ unwords
              ( "dmenu_cdpath"
                : "$HOME/Development"
                : "--"
                : unsafeDmenuOpts
              ),
            unwords
              [ "xargs",
                "--no-run-if-empty",
                "--delimiter='\\n'",
                terminal conf,
                "--working-directory"
              ]
          ]

    dmenuOpts :: [String]
    dmenuOpts =
      [ "-fn", "monospace:size=12",
        "-l", "24",
        "-i",
        "-nb", grey0,
        "-nf", grey6,
        "-sb", grey1,
        "-sf", white
      ]

    unsafeDmenuOpts :: [String]
    unsafeDmenuOpts =
      [ "-fn", "monospace:size=12",
        "-l", "24",
        "-i",
        "-nb", translate grey0,
        "-nf", translate grey6,
        "-sb", translate grey1,
        "-sf", translate white
      ]

    commands :: [(String, X ())]
    commands =
      [ ("shrink", sendMessage Shrink),
        ("expand", sendMessage Expand),
        ("refresh", refresh)
      ]

    screencast :: [String] -> X ()
    screencast args = do
      Screencast on <- XS.get
      if on
        then safeSpawn "killall" ["ffmpeg"]
        else safeSpawn "screencast" args
      XS.put $ Screencast (not on)


newtype Screencast = Screencast Bool deriving (Read, Show, Typeable)

instance ExtensionClass Screencast where
  initialValue = Screencast False
  extensionType = PersistentExtension


xPConfig :: XPConfig
xPConfig =
  def
    { position             = Top,
      height               = 25,
      alwaysHighlight      = False,
      promptBorderWidth    = 0,
      promptKeymap         = keymap `M.union` defaultXPKeymap,
      complCaseSensitivity = ComplCaseSensitive False,
      bgColor              = grey0,
      fgColor              = grey5,
      bgHLight             = grey7,
      fgHLight             = grey0,
      font                 = "xft:monospace:size=12"
    }
  where
    keymap = M.fromList $
      [ ((controlMask, xK_p), moveHistory W.focusUp'),
        ((mod1Mask,    xK_p), moveHistory W.focusUp'),
        ((controlMask, xK_n), moveHistory W.focusDown'),
        ((mod1Mask,    xK_n), moveHistory W.focusDown')
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


black, grey0, grey1, grey2, grey3, grey4, grey6, grey5, grey7, white :: String
black = "#161616"
grey0 = "#1c1c1c"
grey1 = "#212121"
grey2 = "#525d6a"
grey3 = "#787888"
grey4 = "#888888"
grey5 = "#aaaaaa"
grey6 = "#a8b8b8"
grey7 = "#dddddd"
white = "#ffffff"

chalk, blue, cyan, orange, red :: String
chalk  = "#c1f0f7"
blue   = "#004466"
cyan   = "#9bd4ff"
orange = "#ffbb49"
red    = "#fe3d2a"
