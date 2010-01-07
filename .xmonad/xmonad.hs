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
import XMonad.Actions.GridSelect
import XMonad.Hooks.FadeInactive
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimplestFloat
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Launch xmonad 
--
main = do
	dzenSbar <- spawnPipe sBarCmd
	dzenConkyTop <- spawnPipe topBarCmd
	dzenConkyBot <- spawnPipe botBarCmd
	spawn "xcompmgr"
        xmonad $ withUrgencyHook NoUrgencyHook $ myDefaults dzenSbar

myDefaults dzenSbar = defaultConfig 
        {
        terminal           = "urxvtc", 
        focusFollowsMouse  = True,
        borderWidth        = 1,
        modMask            = mod4Mask,
        workspaces         = ["main","www","irc","music","float"],
        normalBorderColor  = "#888888",
        focusedBorderColor = "#9c8e2d",
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
        layoutHook         = myLayoutHook,
        manageHook         = myManageHook,
        logHook            = (dynamicLogWithPP $ myDzenPP dzenSbar) >> fadeInactiveLogHook 0.8
        }

-- Layout configuration
--
myLayoutHook = avoidStruts $ onWorkspace "float" simplestFloat $ tiled ||| Mirror tiled ||| Full
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
    , className =? "Gimp"           --> doFloat ]

-- Custom settings
--
myBitmapsDir = "/home/drot/.dzen"
myFont = "-*-anorexia-*-*-*-*-*-*-*-*-*-*-*-*" 

-- Prompt style
--
myXPConfig = defaultXPConfig
        {
	font  = myFont   
	, fgColor = "#888888"
	, bgColor = "#181818"
	, bgHLight = "#181818"
	, fgHLight = "#9c8e2d"
	, position = Top
        }   

-- Status bar
--
sBarCmd = "dzen2  -ta 'l' -w '520' -fg '#888888' -bg '#181818' -fn '"++ myFont ++"'"
topBarCmd = "conky -c .conkytop | dzen2 -w '1024'  -fg '#888888' -bg '#181818' -ta 'r' -x '512' -fn '"++ myFont ++"'"
botBarCmd = "conky -c .conkybot | dzen2 -ta 'l' -y '768' -fg '#888888' -bg '#181818' -fn '"++ myFont ++"'"

-- Pretty printer look
--
myDzenPP dzenSbar = defaultPP 
        {
          ppOutput = hPutStrLn dzenSbar
	, ppTitle = dzenColor "#888888" "" . wrap "^fg(#9c8e2d)<^fg() "" ^fg(#9c8e2d)>^fg()" . shorten 50
	, ppCurrent = wrap "^fg(#9c8e2d)[^fg()^fg(#b0393f)""^fg(#9c8e2d)]^fg()"
	, ppUrgent = wrap "^fg(#51588e)[^fg()^fg(#9c8e2d)""^fg(#51588e)]^fg()"
	, ppSep = " : "
	, ppWsSep = " : "
	, ppLayout = dzenColor "#9c8e2d" "" .
        (\x -> case x of
        "Tall"           -> "^i("++ myBitmapsDir ++"/tall.xbm)"
	"Mirror Tall"    -> "^i("++ myBitmapsDir ++"/mtall.xbm)"
        "Full"           -> "^i("++ myBitmapsDir ++"/full.xbm)"
	"SimplestFloat"  -> "^i("++ myBitmapsDir ++"/float.xbm)"
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

    -- grid select
    , ((modm,               xK_g     ), goToSelected defaultGSConfig)

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
