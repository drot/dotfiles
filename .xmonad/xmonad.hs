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
import XMonad.Util.Scratchpad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare

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

myConfig = defaultConfig { 
  terminal = "urxvtc"
  , focusFollowsMouse = True
  , borderWidth = 1
  , modMask = mod4Mask
  , workspaces = ["1","2","3","4","5","6","7","8","9"]
  , normalBorderColor = "#3F3F3F"
  , focusedBorderColor = "#6F6F6F"
  , keys = myKeys
  , layoutHook = myLayoutHook
  , manageHook = myManageHook
  }

-- Status bar style
--
myPP = defaultPP { 
  ppTitle = xmobarColor "#F0DFAF" "" . wrap "<fc=#DCDCCC><</fc> " " <fc=#DCDCCC>></fc>" . shorten 50
  , ppCurrent = xmobarColor "#F0DFAF" "" . wrap "<fc=#DCDCCC>[</fc>" "<fc=#DCDCCC>]</fc>"
  , ppUrgent = xmobarColor "#CC9393" "" . wrap "<fc=#CC9393>[</fc>" "<fc=#CC9393>]</fc>"
  , ppSep = " <fc=#6F6F6F>:</fc> "
  , ppWsSep = " <fc=#6F6F6F>:</fc> "
  , ppLayout = xmobarColor "#F0DFAF" ""
  , ppSort = fmap (.scratchpadFilterOutWorkspace) getSortByTag
  }

-- Layout configuration
--
myLayoutHook = onWorkspace "3" tile $ onWorkspace "4" float $ 
               tabs ||| tile ||| mtile ||| full ||| float 
  where
    tabs = named "[T]" $ tabbed shrinkText myTabConfig
    tile = named "[]=" $ Tall 1 (3/100) (1/2)
    mtile = named "[M]" $ Mirror tile
    full = named "[ ]" $ Full
    float = named "><>" $ simplestFloat

myManageHook = composeAll [ 
  className =? "MPlayer" --> doFloat
  , className =? "Gimp" --> doFloat 
  , className =? "Conkeror" --> doShift "2"
  , className =? "Emacs" --> doShift "3"
  ] <+> namedScratchpadManageHook myScratch

-- Scratchpad
--
myScratch = [ 
  NS "music" "urxvtc -e ncmpcpp" (title =? "ncmpcpp")
  (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
  ]

-- Tab style
--
myTabConfig = defaultTheme { 
  fontName = myFont
  , decoHeight = 12
  , activeColor = "#1E2320"
  , activeBorderColor = "#6F6F6F"
  , activeTextColor = "#F0DFAF"
  , inactiveColor = "#3F3F3F"
  , inactiveBorderColor = "#3F3F3F"
  , inactiveTextColor = "#DCDCCC"
  , urgentColor = "#3F3F3F#"
  , urgentBorderColor = "#CC9393"
  , urgentTextColor = "#CC9393" 
  }

-- Prompt style
--
myXPConfig = defaultXPConfig { 
  font = myFont
  , fgColor = "#DCDCCC"
  , bgColor = "#3F3F3F"
  , bgHLight = "#1E2320"
  , fgHLight = "#F0DFAF"
  , position = Bottom 
  }

-- Key bindings
--

-- Toggle struts
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- Main key bindings
myKeys x  = M.union (M.fromList (newKeys x)) (keys defaultConfig x)
newKeys conf@(XConfig {XMonad.modMask = modm}) = [

  -- launch terminal
  ((modm, xK_Return), spawn $ XMonad.terminal conf)
  
    -- launch player
  , ((modm, xK_s), namedScratchpadAction myScratch "music")
    
     -- launch prompt
  , ((modm, xK_p), shellPrompt myXPConfig)

    -- focus urgent window
  , ((modm, xK_u), focusUrgent)

    -- swap the focused window and the master window
  , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)
  ]