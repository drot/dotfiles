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
main = xmonad =<< statusBar cmd pp kb conf 
  where
    uhook = withUrgencyHook NoUrgencyHook
    cmd = "xmobar ~/.xmonad/xmobarrc"
    pp = myPP
    kb = toggleStrutsKey
    conf = uhook myConfig

-- Status bar style
--
myPP = defaultPP {
  ppTitle = xmobarColor "#FFB6B0" "" . wrap "<fc=#B6DCFF><</fc> " " <fc=#B6DCFF>></fc>" . shorten 50,
  ppCurrent = xmobarColor "#CEFFAC" "" . wrap "<fc=#B6DCFF>[</fc>" "<fc=#B6DCFF>]</fc>",
  ppUrgent = xmobarColor "#B6DCFF" "" . wrap "<fc=#CEFFAC>[</fc>" "<fc=#CEFFAC>]</fc>",
  ppSep = " <fc=#FFB6B0>:</fc> ",
  ppWsSep = " <fc=#FFB6B0>:</fc> ",
  ppLayout = xmobarColor "#CEFFAC" "" 
  }

-- My defaults
--
myFont = "-*-anorexia-*-*-*-*-*-*-*-*-*-*-*-*"
myConfig = defaultConfig {
  terminal = "urxvtc",
  focusFollowsMouse = True,
  borderWidth = 1,
  modMask = mod4Mask,
  workspaces = ["1","2","3","4","5","6","7","8","9"],
  normalBorderColor = "#7C7C7C",
  focusedBorderColor = "#FFB6B0",
  keys = myKeys,
  mouseBindings = myMouseBindings,
  layoutHook = myLayoutHook,
  manageHook = myManageHook 
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

myManageHook = composeAll [
  className =? "MPlayer" --> doFloat,
  className =? "Gimp"    --> doFloat 
  ]

-- Tab style
--
myTabConfig = defaultTheme {
  fontName = myFont,
  decoHeight = 12,
  activeColor = "#000000",
  activeBorderColor = "#FFB6B0",
  activeTextColor = "#CEFFAC",
  inactiveColor = "#000000",
  inactiveBorderColor = "#7C7C7C",
  inactiveTextColor = "#EEEEEE",
  urgentColor = "#000000",
  urgentBorderColor = "#FFB6B0",
  urgentTextColor = "#B6DCFF" 
  }

-- Prompt style
--
myXPConfig = defaultXPConfig {
  font = myFont,
  fgColor = "#FFFFFF",
  bgColor = "#000000",
  bgHLight = "#000000",
  fgHLight = "#CEFFAC",
  position = Bottom 
  }

-- Key bindings
--

-- Toggle struts
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- Main key bindings
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $ [
  
  -- launch terminal
  ((modm, xK_Return), spawn $ XMonad.terminal conf),
                                                  
  -- launch prompt
  ((modm, xK_r), shellPrompt myXPConfig),
                                                  
  -- close focused window
  ((modm .|. shiftMask, xK_c), kill),
                                                  
  -- focus urgent window
  ((modm, xK_u), focusUrgent),
                                                  
  -- rotate through the available layout algorithms
  ((modm, xK_space), sendMessage NextLayout),
                                                  
  -- reset the layouts on the current workspace to default
  ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf),
                                                  
  -- resize viewed windows to the correct size
  ((modm, xK_n), refresh),
                                                  
  -- move focus to the next window
  ((modm, xK_Tab), windows W.focusDown),
                                                  
  -- move focus to the next window
  ((modm, xK_j), windows W.focusDown),
                                                  
  -- move focus to the previous window
  ((modm, xK_k), windows W.focusUp  ),
                                                  
  -- move focus to the master window
  ((modm, xK_m), windows W.focusMaster),
                                                  
  -- swap the focused window and the master window
  ((modm .|. shiftMask, xK_Return), windows W.swapMaster),
                                                  
  -- swap the focused window with the next window
  ((modm .|. shiftMask, xK_j), windows W.swapDown),
                                                  
  -- swap the focused window with the previous window
  ((modm .|. shiftMask, xK_k), windows W.swapUp),
                                                  
  -- shrink the master area
  ((modm, xK_h), sendMessage Shrink),
                                                  
  -- expand the master area
  ((modm, xK_l), sendMessage Expand),
                                                  
  -- push window back into tiling
  ((modm, xK_t), withFocused $ windows . W.sink),
                                                  
  -- increment the number of windows in the master area
  ((modm, xK_comma ), sendMessage (IncMasterN 1)),
                                                  
  -- deincrement the number of windows in the master area
  ((modm, xK_period), sendMessage (IncMasterN (-1))),
                                                  
  -- quit xmonad
  ((modm .|. shiftMask, xK_q), io (exitWith ExitSuccess)),
                                                  
  -- restart xmonad
  ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart") 
  ]
                                                
                                                ++
                                                
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  
  [ ((m .|. modm, k), windows $ f i) | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9], 
    (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]

-- Mouse bindings
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $ [ 
  
  -- mod-button1, set the window to floating mode and move by dragging
  ((modm, button1), (\w -> focus w >> mouseMoveWindow w 
                           >> windows W.shiftMaster)),
  -- mod-button2, raise the window to the top of the stack
  ((modm, button2), (\w -> focus w >> windows W.shiftMaster)),
                                                      
  -- mod-button3, set the window to floating mode and resize by dragging
  ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                           >> windows W.shiftMaster)) 
  ]
