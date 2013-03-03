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
import XMonad.Hooks.ManageHelpers
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare

-- Layouts
import XMonad.Layout.Renamed
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimplestFloat

-- Color theme
import Solarized

-- Launch xmonad
--
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myUhook

-- My defaults
--
myBar = "xmobar ~/.xmonad/xmobarrc"

myFont = "-xos4-terminus-medium-*-*-*-14-*-*-*-*-*-iso10646-1"

myUhook = withUrgencyHookC NoUrgencyHook myUrgent myConfig

myConfig = defaultConfig {
             terminal = "urxvtc"
           , focusFollowsMouse = False
           , borderWidth = 2
           , modMask = mod4Mask
           , workspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
           , normalBorderColor = solarizedFG
           , focusedBorderColor = solarizedGreen
           , layoutHook = myLayoutHook
           , manageHook = myManageHook <+> namedScratchpadManageHook myScratch
           }
           `additionalKeysP` myKeys

-- Status bar style
--
myPP = defaultPP {
         ppTitle = xmobarColor solarizedGreen "" . wrap ("<fc=" ++ solarizedBlue ++ ">[</fc>") ("<fc=" ++ solarizedBlue ++ ">]</fc>") . shorten 50
       , ppCurrent = xmobarColor solarizedGreen "" . wrap ("<fc=" ++ solarizedBlue ++ ">[</fc>") ("<fc=" ++ solarizedBlue ++ ">]</fc>")
       , ppUrgent = xmobarColor solarizedBlue "" . wrap ("<fc=" ++ solarizedGreen ++ ">[</fc>") ("<fc=" ++ solarizedGreen ++ ">]</fc>")
       , ppSep = "<fc=" ++ solarizedGreen ++ ">:</fc>"
       , ppWsSep = "<fc=" ++ solarizedGreen ++ ">:</fc>"
       , ppLayout = xmobarColor solarizedBlue ""
       , ppSort = fmap (.scratchpadFilterOutWorkspace) getSortByTag
       }

-- Layout configuration
--
myLayoutHook = onWorkspace "3" tile $ onWorkspace "4" float $
               tabs ||| tile ||| mtile ||| full ||| float
    where
      tabs = renamed [Replace "[T]"] $ tabbed shrinkText myTabConfig
      tile = renamed [Replace "[]="] $ Tall 1 (3/100) (1/2)
      mtile = renamed [Replace "[M]"] $ Mirror tile
      full = renamed [Replace "[ ]"] $ Full
      float = renamed [Replace "><>"] $ simplestFloat

myManageHook = composeAll [
                isFullscreen --> doFullFloat
               , className =? "mplayer2" --> doFloat
               , className =? "Gimp" --> doFloat
               , className =? "Skype" --> doFloat
               , className =? "Conkeror" --> doShift "2"
               , className =? "Emacs" --> doShift "3"
               ]

-- Urgent notification
--
myUrgent = urgencyConfig {
             suppressWhen = Focused
           , remindWhen = Dont
           }

-- Scratchpad
--
myScratch = [ NS "music" "urxvtc -e ncmpcpp" (title =? "ncmpcpp")
              (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
            ]

-- Tab style
--
myTabConfig = defaultTheme {
                fontName = myFont
              , decoHeight = 28
              , activeColor = solarizedBG
              , activeBorderColor = solarizedGreen
              , activeTextColor = solarizedGreen
              , inactiveColor = solarizedBG
              , inactiveBorderColor = solarizedFG
              , inactiveTextColor = solarizedFG
              , urgentColor = solarizedBG
              , urgentBorderColor = solarizedBlue
              , urgentTextColor = solarizedBlue
              }

-- Prompt style
--
myXPConfig = defaultXPConfig {
               font = myFont
             , fgColor = solarizedFG
             , bgColor = solarizedBG
             , bgHLight = solarizedBG
             , fgHLight = solarizedGreen
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
