-------------------------------------------------
--   __   __                                _  --
--   \ \ / /                               | | --
--    \ V / _ __ ___   ___  _ __   __ _  __| | --      
--     > < | '_ ` _ \ / _ \| '_ \ / _` |/ _` | --
--    / . \| | | | | | (_) | | | | (_| | (_| | --
--   /_/ \_\_| |_| |_|\___/|_| |_|\__,_|\__,_| --
-------------------------------------------------                                
-- Imports
import XMonad
import XMonad.Operations
import System.IO
import System.Exit
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad
import XMonad.Util.EZConfig(additionalKeys)

import Graphics.X11.ExtraTypes.XF86
import XMonad.Actions.CycleWS
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks    -- dock/tray mgmt
import Data.Monoid
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import System.Exit
--Layouts
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.Fullscreen
import XMonad.Layout.ToggleLayouts          -- Full window at any time
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Mosaic
import XMonad.Layout.ThreeColumns
-- The main function.
main = do
    cnf <- statusBar myBar myPP toggleStrutsKey myConfig
    ( xmonad . ewmh) cnf

-- Command to launch the bar.
myBar = "xmobar -x0 /home/mike/.xmonad/xmobarrc"
myTerminal = "urxvt"
myBrowser = "qutebrowser"
-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP { ppVisible = xmobarColor "#7F7F7F" "", 
                  ppTitle = xmobarColor "#222222" "", 
		  ppCurrent = xmobarColor "#2E9AFE" "",
                  ppHidden  = xmobarColor "#7F7F7F" "",
                  ppLayout = xmobarColor"#7F7F7F" "", 
                  ppUrgent = xmobarColor "#900000" "" . wrap "[" "]" 
                }


-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- Main configuration, override the defaults to your liking.
myConfig = def { modMask= mod1Mask
                         , terminal = myTerminal
                         , workspaces = myWorkspaces
                         , keys = myKeys
                         , layoutHook = smartBorders $ myLayoutHook
                         , focusedBorderColor = "#2E9AFE"
                         , normalBorderColor = "#000000"
                         , mouseBindings = myMouseBindings                           
			 , manageHook = myManageHook <+> manageHook def
                         , borderWidth         = 1
                         , startupHook = myStartupHook
                         }

--myWorkspaces    = ["1:Web","2:term","3:mail","4:files","5:steam","6","7","8","9"]
xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x    = [x]
myWorkspaces            :: [String]
myWorkspaces            = clickable . (map xmobarEscape) $ ["1:\xf269","2:\xf120","3:\xf0e0", "4:\xf07c","5:\xf1b6","6:\xf281","7:\xf04b","8:\xf167","9"]
                                                                              
  where                                                                       
         clickable l = [ "<action=xdotool key alt+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                             (i,ws) <- zip [1..9] l,                                        
                            let n = i ]
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- launch a terminal
    [ ((modMask,              xK_Return), spawn myTerminal)
 
    -- launch dmenu
    , ((modMask,               xK_d     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
 
    , ((modMask,               xK_i     ), spawn myBrowser)
    -- launch gmrun
    , ((modMask .|. shiftMask, xK_p     ), spawn "rofi -show")
   -- close focused window    
    , ((modMask .|. shiftMask, xK_q     ), kill)
-- switch keyboard layout
    , ((modMask .|. mod4Mask,               xK_u     ), spawn "setxkbmap -layout us")
    , ((modMask .|. mod4Mask, xK_d     ), spawn "setxkbmap -layout dvorak") 
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
    ,((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
    , ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 5%- unmute")
    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 5%+ unmute")
    
    -- Brightness Control
    , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10")
    , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10")
 
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
     , ((modMask              , xK_b     ), sendMessage ToggleStruts)
 
    -- Quit xmonad
    , ((modMask .|. shiftMask, xK_c     ), io (exitWith ExitSuccess))
 
    -- Restart xmonad
    , ((modMask              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    , ((modMask              , xK_o    ), namedScratchpadAction myScratchPads "terminal")
    , ((modMask		     , xK_z    ), namedScratchpadAction myScratchPads "music")
    , ((modMask              , xK_f), sendMessage (Toggle "Full"))
----BSP Layout
    , ((modMask .|. mod4Mask,               xK_l     ), sendMessage $ ExpandTowards R)
    , ((modMask .|. mod4Mask,               xK_h     ), sendMessage $ ExpandTowards L)
    , ((modMask .|. mod4Mask,               xK_j     ), sendMessage $ ExpandTowards D)
    , ((modMask .|. mod4Mask,               xK_k     ), sendMessage $ ExpandTowards U)
--    , ((modMask .|. mod4Mask .|. ctrlMask , xK_l     ), sendMessage $ ShrinkFrom R)
--    , ((modMask .|. mod4Mask .|. ctrlMask , xK_h     ), sendMessage $ ShrinkFrom L)
--    , ((modMask .|. mod4Mask .|. ctrlMask , xK_j     ), sendMessage $ ShrinkFrom D)
--    , ((modMask .|. mod4Mask .|. ctrlMask , xK_k     ), sendMessage $ ShrinkFrom U)
    , ((modMask,                           xK_r     ), sendMessage Rotate)
    , ((modMask,                           xK_s     ), sendMessage Swap)
    , ((modMask,                           xK_n     ), sendMessage FocusParent)
   -- , ((modMask .|. ctrlMask,              xK_n     ), sendMessage SelectNode)
    , ((modMask .|. shiftMask,             xK_n     ), sendMessage MoveNode)
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
  spawnOnce "spotify"
  setWMName "LG3D"
  spawnOnce "dropbox"
  spawnOnce "compton -b"
  spawnOnce "redshift-gtk"

--scratchPad = scratchpadSpawnActionTerminal myTerminal
--scratchPad = [NS "spotify" "spotify" (title =? "spotify") defaultFloating]  
-- Scratchpad
--
--manageScratchPad :: ManageHook
--manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
--
--  where
--
--    h = 1     -- terminal height, 100%
--    w = 1       -- terminal width, 100%
--    t = 1 - h   -- distance from top edge, 90%
--    l = 1 - w   -- distance from left edge, 0%
myScratchPads = [ NS "terminal" spawnTerm  findTerm manageTerm
		, NS "music" spawnPav findPav  managePav
		]
	where

    spawnTerm = myTerminal ++  " -name scratchpad -e cmus"
    findTerm = resource =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h -- and I'd like it fixed using the geometry below

	where

        -- reusing these variables is ok since they're confined to their own 
        -- where clauses 
        h = 1       -- height, 10% 
        w = 1         -- width, 100%
        t = 1 - h     -- bottom edge
        l = 1 -w -- centered left/right
    spawnPav = "music"
    findPav = className =? "Spotify"
    managePav = customFloating $ W.RationalRect l t w h -- and I'd like it fixed using the geometry below

	where

        -- reusing these variables is ok since they're confined to their own 
        -- where clauses 
        h = 1      -- height, 10% 
        w = 1         -- width, 100%
        t = 1 -h      -- bottom edge
        l = 1 -w -- centered left/right
 
myManageHook = composeAll
    [ className =? "stalonetray"    --> doIgnore
      , className =? "Steam"        --> doFullFloat
      , className =? "Firefox"      --> doFullFloat
      , title =? "FEZ"              --> doFullFloat
      , title =? "Don't Starve"     --> doFullFloat
--      , className =? "mpv"          --> doFullFloat
      , manageDocks
      , isFullscreen                --> (doF W.focusDown <+> doFullFloat)
    ] <+> namedScratchpadManageHook myScratchPads

-- Mouse bindings
 
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
 
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.shiftMaster))
 
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
 
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

myLayoutHook = avoidStruts (
       toggleLayouts Full (Grid) ||| toggleLayouts Full (ThreeColMid 1 (1/20) (1/2)) ||| simpleTabbed ||| toggleLayouts Full (tiled) ||| Mirror tiled)
        where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio
 
    -- The default number of windows in the master pane
    nmaster = 1
 
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
 
    -- Percent of screen to increment by when resizing panes
delta = 3/100 
