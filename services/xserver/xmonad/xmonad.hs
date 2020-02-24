import XMonad
    (ChangeLayout(NextLayout), Choose, Full, IncMasterN(IncMasterN), Layout,
     ManageHook, Mirror, Resize(Expand, Shrink), Tall, X, XConfig(XConfig),
     (=?), (-->), className, clickJustFocuses, composeAll, doFloat, doIgnore,
     focusedBorderColor, handleEventHook, io, keys, kill, logHook, manageHook,
     modMask, normalBorderColor, resource, screenWorkspace, sendMessage, spawn,
     startupHook, terminal, whenJust, windows, withFocused, workspaces, xmonad
    )
import XMonad.StackSet
    (focusDown, focusUp, focusMaster, shift, swapMaster, swapDown, swapUp,
     sink, greedyView, view
    )
import Graphics.X11
    (KeyMask, KeySym, shiftMask, xK_1, xK_9, xK_d, xK_e, xK_h, xK_j, xK_k, xK_l,
     xK_m, xK_r, xK_t, xK_w, xK_q, xK_Return, xK_Tab, xK_comma, xK_period, xK_space
    )
import Graphics.X11.Xlib.Extras (Event)
import Data.Bits ((.|.))
import Data.Default (def)
import Data.Monoid (All)
import System.Exit (ExitCode(ExitSuccess), exitWith)

import qualified Data.Map as M


myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {modMask = modm}) = M.fromList $
    -- launch/kill
    [ ((modm .|. shiftMask, xK_Return), spawn $ terminal conf)
    , ((modm,               xK_space ), spawn "dmenu_run -fn monospace:size=12 -l 16 -i -nb '#1c1c1c' -nf '#a5adb7' -sb '#1f1f1f' -sf '#c8f5ff'")
    , ((modm .|. shiftMask, xK_d     ), kill)

    -- layout algorithms
    , ((modm .|. shiftMask, xK_space ), sendMessage NextLayout)

    -- focus
    , ((modm,               xK_Tab   ), windows focusDown)
    , ((modm,               xK_j     ), windows focusDown)
    , ((modm,               xK_k     ), windows focusUp  )
    , ((modm,               xK_m     ), windows focusMaster)

    -- swap
    , ((modm,               xK_Return), windows swapMaster)
    , ((modm .|. shiftMask, xK_j     ), windows swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows swapUp    )

    -- resize
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)

    -- tile
    , ((modm,               xK_t     ), withFocused $ windows . sink)

    -- increment/decrement master area
    , ((modm .|. shiftMask, xK_comma ), sendMessage (IncMasterN 1))
    , ((modm .|. shiftMask, xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- quit or restart
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modm              , xK_q     ), spawn "xmonad --recompile && xmonad --restart")
    ]
    ++
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [ ((m .|. modm, k), windows $ f i)
      | (i, k) <- zip (workspaces conf) [xK_1..xK_9]
      , (f, m) <- [(greedyView, 0), (shift, shiftMask)]
    ]
    ++
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
      , (f, m) <- [(view, 0), (shift, shiftMask)]
    ]

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
myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "vlc"            --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook :: Event -> X All
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook :: X ()
myLogHook = pure ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook :: X ()
myStartupHook = pure ()


main :: IO ()
main = xmonad defaults


defaults :: XConfig (Choose Tall (Choose (Mirror Tall) Full))
defaults = def
    { terminal           = "alacritty"
    , clickJustFocuses   = False
    , normalBorderColor  = "gray13" -- "#212121"
    , focusedBorderColor = "gray29" -- "#4A4A4A"
    , keys               = myKeys
    , manageHook         = myManageHook
    , handleEventHook    = myEventHook
    , logHook            = myLogHook
    , startupHook        = myStartupHook
    }
