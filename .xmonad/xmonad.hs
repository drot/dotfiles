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
import XMonad.Layout.Tabbed
import XMonad.Layout.Renamed
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimplestFloat

-- Font and colors
myFont = "-xos4-terminus-medium-*-*-*-14-*-*-*-*-*-iso8859-2"
myBGColor = "#3f3f3f"
myFGColor = "#dcdccc"
myBlackColor = "2b2b2b"
myRedColor = "#cc9393"
myGreenColor = "#60b48a"
myPurpleColor = "#ec93d3"

-- Launch xmonad
main :: IO ()
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myUhook

-- Override defaults
myConfig = def { terminal = "urxvtc"
               , focusFollowsMouse = False
               , borderWidth = 2
               , modMask = mod4Mask
               , normalBorderColor = myBlackColor
               , focusedBorderColor = myGreenColor
               , layoutHook = myLayoutHook
               , manageHook = myManageHook <+> namedScratchpadManageHook myScratch
               }
           `additionalKeysP` myKeys

-- Spawn status bar
myBar = "xmobar ~/.xmonad/xmobarrc"

-- Wrappers for title and workspaces
myTitleWrap = wrap ("<fc=" ++ myFGColor ++ ">< </fc>") ("<fc=" ++ myFGColor ++ "> ></fc>")
myWorkspaceWrap = wrap ("<fc=" ++ myFGColor ++ ">[</fc>") ("<fc=" ++ myFGColor ++ ">]</fc>")
myUrgentWrap = wrap ("<fc=" ++ myGreenColor ++ ">[</fc>") ("<fc=" ++ myGreenColor ++ ">]</fc>")

-- Urgency hook
myUhook = withUrgencyHookC NoUrgencyHook myUrgent myConfig

-- Status bar output
myPP = def { ppTitle = xmobarColor myRedColor "" . myTitleWrap . shorten 50
           , ppCurrent = xmobarColor myGreenColor "" . myWorkspaceWrap
           , ppUrgent = xmobarColor myPurpleColor "" . myUrgentWrap
           , ppSep = "<fc=" ++ myRedColor ++ ">:</fc>"
           , ppWsSep = "<fc=" ++ myRedColor ++ ">:</fc>"
           , ppLayout = xmobarColor myGreenColor ""
           , ppSort = fmap (.scratchpadFilterOutWorkspace) getSortByTag
           }

-- Layout configuration
myLayoutHook = onWorkspace "3" tile $ onWorkspace "4" float $
               tabs ||| tile ||| mtile ||| full ||| float
    where
      tabs = renamed [Replace "[T]"] $ tabbed shrinkText myTabConfig
      tile = renamed [Replace "[]="] $ Tall 1 (3/100) (1/2)
      mtile = renamed [Replace "[M]"] $ Mirror tile
      full = renamed [Replace "[ ]"] $ Full
      float = renamed [Replace "><>"] $ smartBorders simplestFloat

-- Window rules
myManageHook = composeAll [ isFullscreen --> doFullFloat
                          , className =? "mpv" --> doFloat
                          , className =? "Gimp" --> doFloat
                          , className =? "Conkeror" --> doShift "2"
                          , className =? "Emacs" --> doShift "3"
                          ]

-- Urgent notification
myUrgent = urgencyConfig { suppressWhen = Focused
                         , remindWhen = Dont
                         }

-- Scratchpad
myScratch = [ NS "music" "urxvtc -e ncmpcpp" (title =? "ncmpcpp")
              (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
            ]

-- Tab style
myTabConfig = def { fontName = myFont
                  , decoHeight = 28
                  , activeColor = myBGColor
                  , activeBorderColor = myGreenColor
                  , activeTextColor = myGreenColor
                  , inactiveColor = myBGColor
                  , inactiveBorderColor = myBlackColor
                  , inactiveTextColor = myFGColor
                  , urgentColor = myBGColor
                  , urgentBorderColor = myPurpleColor
                  , urgentTextColor = myPurpleColor
                  }

-- Prompt style
myXPConfig = def { font = myFont
                 , fgColor = myFGColor
                 , bgColor = myBGColor
                 , bgHLight = myBGColor
                 , fgHLight = myGreenColor
                 , position = Bottom
                 }

-- Toggle struts
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- Override default keybindings
myKeys = [ ("M-<Return>", spawn $ XMonad.terminal myConfig)
         , ("M-s", namedScratchpadAction myScratch "music")
         , ("M-p", shellPrompt myXPConfig)
         , ("M-u", focusUrgent)
         , ("M-S-<Return>", windows W.swapMaster)
         ]
