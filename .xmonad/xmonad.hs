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

-- Font and colors
--
myFont = "-xos4-terminus-medium-*-*-*-14-*-*-*-*-*-iso8859-2"
myBGColor = "#3f3f3f"
myFGColor = "#dcdccc"
myRedColor = "#dca3a3"
myBlackColor = "#1E2320"
myYellowColor = "#f0dfaf"
myPurpleColor = "#ec93d3"

-- Launch xmonad
--
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myUhook

-- Override defaults
myConfig = defaultConfig {
             terminal = "urxvtc"
           , focusFollowsMouse = False
           , borderWidth = 2
           , modMask = mod4Mask
           , normalBorderColor = myFGColor
           , focusedBorderColor = myRedColor
           , layoutHook = myLayoutHook
           , manageHook = myManageHook <+> namedScratchpadManageHook myScratch
           }
           `additionalKeysP` myKeys

-- Status bar
--

-- Spawn status bar
myBar = "xmobar ~/.xmonad/xmobarrc"

-- Wrappers for title and workspaces
myTitleWrap = wrap ("<fc=" ++ myFGColor ++ ">< </fc>") ("<fc=" ++ myFGColor ++ "> ></fc>")
myWorkspaceWrap = wrap ("<fc=" ++ myFGColor ++ ">[</fc>") ("<fc=" ++ myFGColor ++ ">]</fc>")
myUrgentWrap = wrap ("<fc=" ++ myYellowColor ++ ">[</fc>") ("<fc=" ++ myYellowColor ++ ">]</fc>")

-- Urgency hook
myUhook = withUrgencyHookC NoUrgencyHook myUrgent myConfig

-- Status bar output
myPP = defaultPP {
         ppTitle = xmobarColor myRedColor "" . myTitleWrap . shorten 50
       , ppCurrent = xmobarColor myYellowColor "" . myWorkspaceWrap
       , ppUrgent = xmobarColor myPurpleColor "" . myUrgentWrap
       , ppSep = "<fc=" ++ myRedColor ++ ">:</fc>"
       , ppWsSep = "<fc=" ++ myRedColor ++ ">:</fc>"
       , ppLayout = xmobarColor myYellowColor ""
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

-- Window rules
--
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
              , activeColor = myBlackColor
              , activeBorderColor = myYellowColor
              , activeTextColor = myYellowColor
              , inactiveColor = myBGColor
              , inactiveBorderColor = myFGColor
              , inactiveTextColor = myFGColor
              , urgentColor = myBGColor
              , urgentBorderColor = myPurpleColor
              , urgentTextColor = myPurpleColor
              }

-- Prompt style
--
myXPConfig = defaultXPConfig {
               font = myFont
             , fgColor = myFGColor
             , bgColor = myBGColor
             , bgHLight = myBlackColor
             , fgHLight = myYellowColor
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
