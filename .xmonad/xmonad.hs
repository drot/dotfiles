--
-- xmonad by drot
--

import XMonad
import System.Exit

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleFloat
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Launch xmonad 
--
main = do
	h <- spawnPipe "xmobar /home/drot/.xmonad/xmobarrc"
        xmonad $ withUrgencyHook NoUrgencyHook $ myDefaults h

myDefaults h = defaultConfig {
        terminal           = "urxvt", 
        focusFollowsMouse  = True,
        borderWidth        = 1,
        modMask            = mod4Mask,
        workspaces         = ["main","www","irc","music","misc"],
        normalBorderColor  = "#181818",
        focusedBorderColor = "#888888",
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
        layoutHook         = myLayoutHook,
        manageHook         = myManageHook,
        logHook            = dynamicLogWithPP $ myBar h 
    }

-- Layout configuration
--
myLayoutHook = avoidStruts $  onWorkspace "misc" simpleFloat $ tiled ||| Mirror tiled ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Wine"           --> doFloat
    , className =? "Gimp"           --> doFloat ]

-- Prompt style
--
myXPConfig = defaultXPConfig
    {
	font  = "-*-anorexia-*-*-*-*-*-*-*-*-*-*-*-*"
	, fgColor = "#888888"
	, bgColor = "#181818"
	, bgHLight = "#181818"
	, fgHLight = "#9c8e2d"
	, position = Top
    }

-- Status bar style
--
myBar h = defaultPP { 
          ppOutput = hPutStrLn h
	, ppTitle = xmobarColor "#888888" "" . shorten 50
	, ppCurrent = xmobarColor "#b0393f" "" . wrap "<fc=#9c8e2d>[</fc>" "<fc=#9c8e2d>]</fc>"
	, ppUrgent = xmobarColor "#b0393f" "" . wrap "<fc=#51588e>[</fc>" "<fc=#51588e>]</fc>"
	, ppSep = " : "
	, ppWsSep = " : "
	, ppLayout = xmobarColor "#9c8e2d" "" .
        (\x -> case x of
        "Tall"           -> "[]="
	"Mirror Tall"    -> "=--"
        "Full"           -> "[M]"
	"Simple Float"   -> "><>"
        _                -> x
        )
                    }
------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch prompt
    , ((modm,               xK_p     ), shellPrompt myXPConfig) 

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
