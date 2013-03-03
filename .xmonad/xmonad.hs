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
           , normalBorderColor = "#839496"
           , focusedBorderColor = "#859900"
           , layoutHook = myLayoutHook
           , manageHook = myManageHook <+> namedScratchpadManageHook myScratch
           }
           `additionalKeysP` myKeys

-- Status bar style
--
myPP = defaultPP {
         ppTitle = xmobarColor "#859900" "" . wrap "<fc=#268bd2><</fc> " " <fc=#268bd2>></fc>" . shorten 50
       , ppCurrent = xmobarColor "#859900" "" . wrap "<fc=#268bd2>[</fc>" "<fc=#268bd2>]</fc>"
       , ppUrgent = xmobarColor "#268bd2" "" . wrap "<fc=#859900>[</fc>" "<fc=#859900>]</fc>"
       , ppSep = "<fc=#859900>:</fc>"
       , ppWsSep = "<fc=#859900>:</fc>"
       , ppLayout = xmobarColor "#268bd2" ""
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
              , activeColor = "#002b36"
              , activeBorderColor = "#859900"
              , activeTextColor = "#859900"
              , inactiveColor = "#002b36"
              , inactiveBorderColor = "#839496"
              , inactiveTextColor = "#839496"
              , urgentColor = "#002b36"
              , urgentBorderColor = "#268bd2"
              , urgentTextColor = "#268bd2"
              }

-- Prompt style
--
myXPConfig = defaultXPConfig {
               font = myFont
             , fgColor = "#839496"
             , bgColor = "#002b36"
             , bgHLight = "#002b36"
             , fgHLight = "#859900"
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
