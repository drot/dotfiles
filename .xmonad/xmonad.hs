--
-- xmonad by drot
--

-- Main
import XMonad
import System.Exit
import qualified Data.Map as M
import qualified XMonad.StackSet as W

-- General
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks

-- Layouts
import XMonad.Layout.Named
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimplestFloat

-- Launch xmonad 
--
main = xmonad =<< statusBar cmd pp kb conf
	where
		uhook = withUrgencyHook NoUrgencyHook
    		cmd = "xmobar ~/.xmonad/xmobarrc"
		pp = myPP
		kb = toggleStrutsKey
		conf = uhook myConfig

-- Status bar style
--
myPP = defaultPP
	{ 
		ppTitle = xmobarColor "#FFB6B0" "" . wrap "<fc=#B6DCFF><</fc> " " <fc=#B6DCFF>></fc>" . shorten 50
		, ppCurrent = xmobarColor "#CEFFAC" "" . wrap "<fc=#B6DCFF>[</fc>" "<fc=#B6DCFF>]</fc>"
		, ppUrgent = xmobarColor "#B6DCFF" "" . wrap "<fc=#CEFFAC>[</fc>" "<fc=#CEFFAC>]</fc>"
		, ppSep = " <fc=#FFB6B0>:</fc> "
		, ppWsSep = " <fc=#FFB6B0>:</fc> "
		, ppLayout = xmobarColor "#CEFFAC" ""
	} 

-- My defaults
--
myFont = "-*-anorexia-*-*-*-*-*-*-*-*-*-*-*-*"
myConfig = defaultConfig
	{ 
		terminal = "urxvtc" 
		, focusFollowsMouse = True
		, borderWidth = 1
		, modMask = mod4Mask
		, workspaces = ["1","2","3","4","5","6","7","8","9"]
		, normalBorderColor = "#7C7C7C"
		, focusedBorderColor = "#FFB6B0"
		, keys = myKeys
		, mouseBindings = myMouseBindings
		, layoutHook = myLayoutHook
		, manageHook = myManageHook
	}    

-- Layout configuration
--
myLayoutHook = onWorkspaces ["5", "6"] float $ tabs ||| tile ||| mtile ||| full
	where
		tabs = named "[T]" $ tabbed shrinkText myTabConfig
		float = named "><>" $ simplestFloat
		tile = named "[]=" $ Tall 1 (3/100) (1/2)
		mtile = named "[M]" $ Mirror tile
		full = named "[ ]" $ Full

myManageHook = composeAll
	[ className =? "MPlayer" --> doFloat
	, className =? "Gimp"    --> doFloat ]

-- Tab style
--
myTabConfig = defaultTheme
	{
		fontName = myFont
		, decoHeight = 12
		, activeColor = "#000000"
		, activeBorderColor = "#7C7C7C"
		, activeTextColor = "#CEFFAC"
		, inactiveColor = "#000000"
		, inactiveBorderColor = "#7C7C7C"
		, inactiveTextColor = "#EEEEEE"
		, urgentColor = "#000000"
		, urgentBorderColor = "#7C7C7C"
		, urgentTextColor = "#B6DCFF"
	}

-- Prompt style
--
myXPConfig = defaultXPConfig 
	{
		font  = myFont
		, fgColor = "#888888"
		, bgColor = "#181818"
		, bgHLight = "#9c8e2d"
		, fgHLight = "#181818"
		, position = Bottom
	}

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--

-- Toggle struts
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- Main key bindings
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch prompt
    , ((modm,               xK_p     ), shellPrompt myXPConfig) 

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

    -- focus urgent window
    , ((modm,               xK_u ), focusUrgent)

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
