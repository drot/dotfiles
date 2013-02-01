--
-- xmonad by drot
--

-- Main
import XMonad
import qualified XMonad.StackSet as W

-- General
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig
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

myFont = "-xos4-terminus-medium-*-*-*-12-*-*-*-*-*-iso10646-1"

myUhook = withUrgencyHook NoUrgencyHook myConfig

myConfig = defaultConfig {
             terminal = "urxvtc"
           , focusFollowsMouse = False
           , borderWidth = 2
           , modMask = mod4Mask
           , workspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
           , normalBorderColor = "#3F3F3F"
           , focusedBorderColor = "#6F6F6F"
           , layoutHook = myLayoutHook
           , manageHook = myManageHook <+> namedScratchpadManageHook myScratch
           } `additionalKeysP` myKeys

-- Status bar style
--
myPP = defaultPP {
         ppTitle = xmobarColor "#F0DFAF" "" . wrap "<fc=#DCDCCC><</fc> " " <fc=#DCDCCC>></fc>" . shorten 80
       , ppCurrent = xmobarColor "#F0DFAF" "" . wrap "<fc=#DCDCCC>[</fc>" "<fc=#DCDCCC>]</fc>"
       , ppUrgent = xmobarColor "#EC93D3" "" . wrap "<fc=#F0DFAF>[</fc>" "<fc=#F0DFAF>]</fc>"
       , ppSep = "<fc=#DCA3A3>:</fc>"
       , ppWsSep = "<fc=#DCA3A3>:</fc>"
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
               , className =? "Skype" --> doFloat
               , className =? "Firefox" --> doShift "2"
               , className =? "Emacs" --> doShift "3"
               ]

-- Scratchpad
--
myScratch = [ NS "music" "urxvtc -e ncmpcpp" (title =? "ncmpcpp")
              (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
            ]

-- Tab style
--
myTabConfig = defaultTheme {
                fontName = myFont
              , activeColor = "#1E2320"
              , activeBorderColor = "#6F6F6F"
              , activeTextColor = "#F0DFAF"
              , inactiveColor = "#3F3F3F"
              , inactiveBorderColor = "#3F3F3F"
              , inactiveTextColor = "#DCDCCC"
              , urgentColor = "#3F3F3F#"
              , urgentBorderColor = "#EC93D3"
              , urgentTextColor = "#EC93D3"
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

-- Override defaults
myKeys = [ ("M-<Return>", spawn $ XMonad.terminal myConfig)
         , ("M-s", namedScratchpadAction myScratch "music")
         , ("M-p", shellPrompt myXPConfig)
         , ("M-u", focusUrgent)
         , ("M-S-<Return>", windows W.swapMaster)
         ]
