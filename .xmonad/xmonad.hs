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
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myUhook

-- My defaults
--
myBar = "xmobar ~/.xmonad/xmobarrc"

myFont = "-*-anorexia-*-*-*-*-*-*-*-*-*-*-*-*"

myUhook = withUrgencyHook NoUrgencyHook myConfig

myConfig = defaultConfig { terminal = "urxvtc"
			 , focusFollowsMouse = True
			 , borderWidth = 1
			 , modMask = mod4Mask
			 , workspaces = ["1","2","3","4","5","6","7","8","9"]
			 , normalBorderColor = "#7C7C7C"
			 , focusedBorderColor = "#FFB6B0"
			 , keys = myKeys
			 , layoutHook = myLayoutHook
			 , manageHook = myManageHook 
			 }

-- Status bar style
--
myPP = defaultPP { ppTitle = xmobarColor "#FFB6B0" "" . wrap "<fc=#B6DCFF><</fc> " " <fc=#B6DCFF>></fc>" . shorten 50
		 , ppCurrent = xmobarColor "#CEFFAC" "" . wrap "<fc=#B6DCFF>[</fc>" "<fc=#B6DCFF>]</fc>"
		 , ppUrgent = xmobarColor "#B6DCFF" "" . wrap "<fc=#CEFFAC>[</fc>" "<fc=#CEFFAC>]</fc>"
		 , ppSep = " <fc=#FFB6B0>:</fc> "
		 , ppWsSep = " <fc=#FFB6B0>:</fc> "
		 , ppLayout = xmobarColor "#CEFFAC" ""
		 }

-- Layout configuration
--
myLayoutHook = onWorkspaces ["4","5","6"] float $ 
		tabs ||| tile ||| mtile ||| full ||| float 
	where
    		tabs = named "[T]" $ tabbed shrinkText myTabConfig
    		tile = named "[]=" $ Tall 1 (3/100) (1/2)
    		mtile = named "[M]" $ Mirror tile
    		full = named "[ ]" $ Full
		float = named "><>" $ simplestFloat

myManageHook = composeAll [ className =? "MPlayer" --> doFloat
                          , className =? "Gimp" --> doFloat 
                          ]	

-- Tab style
--
myTabConfig = defaultTheme { fontName = myFont
			   , decoHeight = 12
			   , activeColor = "#000000"
			   , activeBorderColor = "#FFB6B0"
			   , activeTextColor = "#CEFFAC"
			   , inactiveColor = "#000000"
			   , inactiveBorderColor = "#7C7C7C"
			   , inactiveTextColor = "#EEEEEE"
			   , urgentColor = "#000000"
			   , urgentBorderColor = "#FFB6B0"
			   , urgentTextColor = "#B6DCFF" 
			   }

-- Prompt style
--
myXPConfig = defaultXPConfig { font = myFont
			     , fgColor = "#FFFFFF"
			     , bgColor = "#000000"
			     , bgHLight = "#000000"
			     , fgHLight = "#CEFFAC"
			     , position = Bottom 
			     }

-- Key bindings
--

-- Toggle struts
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- Main key bindings
myKeys x  = M.union (M.fromList (newKeys x)) (keys defaultConfig x)
newKeys conf@(XConfig {XMonad.modMask = modm}) =

	-- launch terminal
	[ ((modm, xK_Return), spawn $ XMonad.terminal conf)

	-- launch prompt
	, ((modm, xK_p), shellPrompt myXPConfig)

	-- focus urgent window
	, ((modm, xK_u), focusUrgent)

	-- swap the focused window and the master window
	, ((modm .|. shiftMask, xK_Return), windows W.swapMaster)
	]
