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
           , normalBorderColor = "#7C7C7C"
           , focusedBorderColor = "#FFB6B0"
           , layoutHook = myLayoutHook
           , manageHook = myManageHook <+> namedScratchpadManageHook myScratch
           } `additionalKeysP` myKeys

-- Status bar style
--
myPP = defaultPP {
         ppTitle = xmobarColor "#FFB6B0" "" . wrap "<fc=#B6DCFF><</fc> " " <fc=#B6DCFF>></fc>" . shorten 80
       , ppCurrent = xmobarColor "#CEFFAC" "" . wrap "<fc=#B6DCFF>[</fc>" "<fc=#B6DCFF>]</fc>"
       , ppUrgent = xmobarColor "#B6DCFF" "" . wrap "<fc=#CEFFAC>[</fc>" "<fc=#CEFFAC>]</fc>"
       , ppSep = "<fc=#FFB6B0>:</fc>"
       , ppWsSep = "<fc=#FFB6B0>:</fc>"
       , ppLayout = xmobarColor "#CEFFAC" ""
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
              , decoHeight = 14
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
myXPConfig = defaultXPConfig {
               font = myFont
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

-- Override defaults
myKeys = [ ("M-<Return>", spawn $ XMonad.terminal myConfig)
         , ("M-s", namedScratchpadAction myScratch "music")
         , ("M-p", shellPrompt myXPConfig)
         , ("M-u", focusUrgent)
         , ("M-S-<Return>", windows W.swapMaster)
         ]
