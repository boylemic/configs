
-- Imports.
import XMonad
import XMonad.Operations
import System.IO
import System.Exit
import XMonad.Util.Run
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks    -- dock/tray mgmt
import Data.Monoid
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import System.Exit
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.Fullscreen
import XMonad.Util.SpawnOnce
--import System.Taffybar.XMonadLog ( dbusLog )

-- The main function.
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

-- Command to launch the bar.
myBar = "xmobar"


-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP { ppVisible = xmobarColor "#404040" "", 
                  ppCurrent = xmobarColor "#0080FF" "",  
                  ppTitle = xmobarColor "#FFB6B0" "", 
  --                ppHiddenNoWindows = xmobarColor "#222222" "", 
   --               ppLayout = xmobarColor"#790a0a" "", 
                  ppUrgent = xmobarColor "#900000" "" . wrap "[" "]" 
                }


-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- Main configuration, override the defaults to your liking.
myConfig = def { modMask= mod1Mask
                         , terminal = "urxvt"
                         , workspaces = myWorkspaces
                         , keys = myKeys
                         , layoutHook = smartBorders $ myLayoutHook
                         , focusedBorderColor = "#2E9AFE"
                         , normalBorderColor = "#000000"
                         , manageHook = myManageHook <+> manageHook def
                         , mouseBindings = myMouseBindings
                         , borderWidth         = 0
                         , startupHook = myStartupHook
                         }

--myWorkspaces    = ["1:Web","2:term","3:mail","4:files","5:steam","6","7","8","9"]
xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x    = [x]
myWorkspaces            :: [String]
myWorkspaces            = clickable . (map xmobarEscape) $ ["1","2","3","4","5","6","7","8","9"]
                                                                              
  where                                                                       
         clickable l = [ "<action=xdotool key alt+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                             (i,ws) <- zip [1..9] l,                                        
                            let n = i ]
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- launch a terminal
    [ ((mod1Mask,              xK_Return), spawn "urxvt")
 
    -- launch dmenu
    , ((modMask,               xK_d     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
 
    -- launch gmrun
    , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun")
 
    -- close focused window
    , ((modMask .|. shiftMask, xK_q     ), kill)
 
     -- Rotate through the available layout algorithms
    , ((modMask,               xK_space ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
    , ((modMask,               xK_n     ), refresh)
 
    -- Move focus to the next window
    , ((modMask,               xK_Tab   ), windows W.focusDown)
 
    -- Move focus to the next window
    , ((modMask,               xK_j     ), windows W.focusDown)
 
    -- Move focus to the previous window
    , ((modMask,               xK_k     ), windows W.focusUp  )
    
    -- Volume Control
    , ((modMask,               xK_F11   ), spawn "amixer set Master 5%-")
    , ((modMask,               xK_F12   ), spawn "amixer set Master 5%+")
    
    -- Brightness Control
    , ((modMask,               xK_F4    ), spawn "xbacklight -dec 10")
    , ((modMask,               xK_F5    ), spawn "xbacklight -inc 10")
 
    -- Move focus to the master window
    , ((modMask,               xK_m     ), windows W.focusMaster  )
 
    -- Swap the focused window and the master window
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
 
    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
 
    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )
 
    -- Shrink the master area
    , ((modMask,               xK_h     ), sendMessage Shrink)
 
    -- Expand the master area
    , ((modMask,               xK_l     ), sendMessage Expand)
 
    -- Push window back into tiling
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)
 
    -- Increment the number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
 
    -- Deincrement the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))
 
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)
 
    -- Quit xmonad
    , ((modMask .|. shiftMask, xK_c     ), io (exitWith ExitSuccess))
 
    -- Restart xmonad
    , ((modMask              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++
 
    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
 
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
---spawn
--
myStartupHook = do
  spawnOnce "/usr/bin/stalonetray"
  spawnOnce "nm-applet"
  spawnOnce "volumeicon"
  spawnOnce "dropbox"
  spawnOnce "compton -cb"



myManageHook = composeAll
    [ className =? "stalonetray"    --> doIgnore
      , className =? "Steam"        --> doFullFloat
      , title =? "LIMBO"            --> doIgnore
      , title =? "FEZ"              --> doIgnore
      , title =? "NMRIH"            --> doFullFloat
      , title =? "Portal"            --> doFullFloat
      , className =? "firefox"      --> doFullFloat
      , className =? "mpv"          --> doFullFloat
      , manageDocks
      , isFullscreen                --> (doF W.focusDown <+> doFullFloat)
    ]

-- Mouse bindings
 
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
 
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
 
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
 
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
 
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

myLayoutHook = avoidStruts (
        Grid ||| tiled |||noBorders (fullscreenFull Full) ||| noBorders simpleTabbed ||| Mirror tiled)
        where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio
 
    -- The default number of windows in the master pane
    nmaster = 1
 
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
 
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100 
