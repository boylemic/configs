-- Imports.
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks    -- dock/tray mgmt
import Data.Monoid
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import System.Exit
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
-- The main function.
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

-- Command to launch the bar.
myBar = "xmobar"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "" "" }

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- Main configuration, override the defaults to your liking.
myConfig = defaultConfig { modMask= mod1Mask
			 , terminal = "urxvt"
			 , workspaces = myWorkspaces
			 , keys = myKeys
			 , layoutHook = myLayoutHook
                         , focusedBorderColor = "#2E9AFE"
                         , normalBorderColor = "#000000"
			 , manageHook = myManageHook
			 }

myWorkspaces    = ["1:www","2:term","3:mail","4:files","5:steam","6","7","8","9"]
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- launch a terminal
    [ ((mod1Mask .|. shiftMask, xK_Return), spawn "urxvt")
 
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
    , ((modMask,               xK_Return), windows W.swapMaster)
 
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
    , ((modMask .|. shiftMask, xK_e     ), io (exitWith ExitSuccess))
 
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

myManageHook = composeAll
    [ className =? "stalonetray"    --> doIgnore
    
    ]

myLayoutHook = noBorders Full ||| noBorders simpleTabbed ||| Grid


