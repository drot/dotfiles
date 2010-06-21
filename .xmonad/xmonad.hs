--
-- xmonad by drot
--

-- Main
import XMonad
import System.Exit
import qualified Data.Map as M
import qualified XMonad.StackSet as W

-- Utils
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks

-- Layouts
import XMonad.Layout.Named
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Tabbed

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
		ppTitle = xmobarColor "#9c8e2d" "" . wrap "<fc=#51588e><</fc> " " <fc=#51588e>></fc>" . shorten 50
		, ppCurrent = xmobarColor "#9c8e2d" "" . wrap "<fc=#51588e>[</fc>" "<fc=#51588e>]</fc>"
		, ppUrgent = xmobarColor "#51588e" "" . wrap "<fc=#9c8e2d>[</fc>" "<fc=#9c8e2d>]</fc>"
		, ppSep = " <fc=#9c8e2d>:</fc> "
		, ppWsSep = " <fc=#9c8e2d>:</fc> "
		, ppLayout = xmobarColor "#9c8e2d" ""
	} 

-- My defaults
--
myConfig = defaultConfig
	{ 
		terminal = "urxvtc" 
		, focusFollowsMouse = True
		, borderWidth = 1
		, modMask = mod4Mask
		, workspaces = ["1","2","3","4","5","6","7","8","9"]
		, normalBorderColor = "#888888"
		, focusedBorderColor = "#9c8e2d"
		, keys = myKeys
		, mouseBindings = myMouseBindings
		, layoutHook = myLayoutHook
		, manageHook = myManageHook
	}    

-- Layout configuration
--
myLayoutHook = onWorkspace "2" tabs $ onWorkspaces ["4","5","6"] float $ tile ||| mtile ||| tabs ||| full
  where
    tile = named "[]=" $ Tall 1 (3/100) (1/2)
    mtile = named "[M]" $ Mirror tile
    float = named "><>" $ simplestFloat
    tabs = named "[=]" $ tabbed shrinkText myTabConfig
    full = named "[ ]" $ Full

myManageHook = composeAll
    [ className =? "MPlayer" --> doFloat
    , className =? "Gimp"    --> doFloat ]

-- Tab style
--
myTabConfig = defaultTheme
	{
		fontName = "-*-lime-*-*-*-*-*-*-*-*-*-*-*-*"
		, decoHeight = 12
		, activeColor = "#a6c292"
		, activeBorderColor = "#a6c292"
		, activeTextColor = "#000000"
		, inactiveBorderColor = "#000000"
	}

-- Prompt style
--
myXPConfig = defaultXPConfig 
	{
		font  = "-*-anorexia-*-*-*-*-*-*-*-*-*-*-*-*"
		, fgColor = "#888888"
		, bgColor = "#181818"
		, bgHLight = "#181818"
		, fgHLight = "#9c8e2d"
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
