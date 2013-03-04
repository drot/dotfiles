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
myBGColor = "#002b36"
myFGColor = "#839496"
myGreenColor = "#859900"
myBlueColor = "#268bd2"

-- Launch xmonad
--
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myUhook

-- Spawn status bar
myBar = "xmobar ~/.xmonad/xmobarrc"

-- Urgency hook
myUhook = withUrgencyHookC NoUrgencyHook myUrgent myConfig

-- Override defaults
myConfig = defaultConfig {
             terminal = "urxvtc"
           , focusFollowsMouse = False
           , borderWidth = 2
           , modMask = mod4Mask
           , workspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
           , normalBorderColor = myFGColor
           , focusedBorderColor = myGreenColor
           , layoutHook = myLayoutHook
           , manageHook = myManageHook <+> namedScratchpadManageHook myScratch
           }
           `additionalKeysP` myKeys

-- Status bar style
--
myPP = defaultPP {
         ppTitle = xmobarColor myGreenColor "" . wrap ("<fc=" ++ myBlueColor ++ ">< </fc>") ("<fc=" ++ myBlueColor ++ "> ></fc>") . shorten 50
       , ppCurrent = xmobarColor myGreenColor "" . wrap ("<fc=" ++ myBlueColor ++ ">[</fc>") ("<fc=" ++ myBlueColor ++ ">]</fc>")
       , ppUrgent = xmobarColor myBlueColor "" . wrap ("<fc=" ++ myGreenColor ++ ">[</fc>") ("<fc=" ++ myGreenColor ++ ">]</fc>")
       , ppSep = "<fc=" ++ myGreenColor ++ ">:</fc>"
       , ppWsSep = "<fc=" ++ myGreenColor ++ ">:</fc>"
       , ppLayout = xmobarColor myBlueColor ""
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

-- Application behavior
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
              , activeColor = myBGColor
              , activeBorderColor = myGreenColor
              , activeTextColor = myGreenColor
              , inactiveColor = myBGColor
              , inactiveBorderColor = myFGColor
              , inactiveTextColor = myFGColor
              , urgentColor = myBGColor
              , urgentBorderColor = myBlueColor
              , urgentTextColor = myBlueColor
              }

-- Prompt style
--
myXPConfig = defaultXPConfig {
               font = myFont
             , fgColor = myFGColor
             , bgColor = myBGColor
             , bgHLight = myBGColor
             , fgHLight = myGreenColor
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
