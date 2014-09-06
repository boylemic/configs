import XMonad hiding ( (|||), Tall )
import XMonad.Hooks.DynamicLog
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Place
import XMonad.Hooks.ManageHelpers
import XMonad.Util.CustomKeys
import XMonad.Util.EZConfig
import XMonad.Layout.Magnifier as Mag
import XMonad.Layout.BoringWindows
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.SimpleFloat
import XMonad.Layout.DragPane
import XMonad.Layout.TwoPane
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Tabbed
import Data.Monoid
import XMonad.Layout.Named
import XMonad.Actions.CycleSelectedLayouts as Cycle
import XMonad.Actions.RotSlaves
import XMonad.Layout.WindowNavigation
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit

main = xmonad $ defaultConfig
                { terminal                                                = "urxvt"
                , modMask                                                               = mod1Mask --rebind Mod to Windows Key
                , manageHook                                            = myManageHook
                , layoutHook                                            = myLayoutHook
                , focusedBorderColor    = "#ee9a00"
                , normalBorderColor                     = "#000000"
                , startupHook                                           = startup
                , workspaces = ["1","2","3","4","5","6","7","8","9"]
                }

                `additionalKeys`
                [ ((mod1Mask .|. shiftMask, xK_Return), spawn "urxvt")
-- launch dmenu
                    , ((mod1Mask, xK_d ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
-- launch gmrun
                    , ((mod1Mask .|. shiftMask, xK_p ), spawn "gmrun")
-- close focused window
                    , ((mod1Mask .|. shiftMask, xK_c ), kill)
                    
                    , ((mod1Mask, xK_n ), refresh)
-- Move focus to the next window
                    , ((mod1Mask, xK_Tab ), windows W.focusDown)
-- Move focus to the next window
                    , ((mod1Mask, xK_j ), windows W.focusDown)
-- Move focus to the previous window
                    , ((mod1Mask, xK_k ), windows W.focusUp )
-- Move focus to the master window
                    , ((mod1Mask, xK_m ), windows W.focusMaster )
-- Swap the focused window and the master window
                    , ((mod1Mask, xK_Return), windows W.swapMaster)
-- Swap the focused window with the next window
                    , ((mod1Mask .|. shiftMask, xK_j ), windows W.swapDown )
-- Swap the focused window with the previous window
                    , ((mod1Mask .|. shiftMask, xK_k ), windows W.swapUp )
-- Shrink the master area
                    , ((mod1Mask, xK_h ), sendMessage Shrink)
-- Expand the master area
                    , ((mod1Mask, xK_l ), sendMessage Expand)
-- Push window back into tiling
                    , ((mod1Mask, xK_t ), withFocused $ windows . W.sink)
-- Increment the number of windows in the master area
                    , ((mod1Mask , xK_comma ), sendMessage (IncMasterN 1))
-- Deincrement the number of windows in the master area
                    , ((mod1Mask , xK_period), sendMessage (IncMasterN (-1)))
-- Toggle the status bar gap
-- Use this binding with avoidStruts from Hooks.ManageDocks.
-- See also the statusBar function from Hooks.DynamicLog.
-- 
-- , ((modm , xK_b ), sendMessage ToggleStruts)
-- Quit xmonad
                    , ((mod1Mask .|. shiftMask, xK_q ), io (exitWith ExitSuccess))
-- Restart xmonad
                    , ((mod1Mask , xK_q ), spawn "xmonad --recompile; xmonad --restart")
                ]

myLayoutHook = windowNavigation . avoidStruts . smartBorders . Mag.maximizeVertical $ (named "default" mouseResizableTile ||| named "two pane" (TwoPane (3/100) (1/2) ) |||  named "full" Full ||| gimpLayout ||| pdfLayout)
        where
        gimpLayout = named "gimp layout" (simpleTabbed ****||* simpleTabbed)
        pdfLayout = named "pdf layout" (simpleTabbed *||* mouseResizableTile)

myManageHook = composeAll $
        [ resource =? name --> doIgnore | name <- ignore ]
        ++[ title =? name --> doCenterFloat | name <- floaters ]
        ++[ resource =? name --> doCenterFloat | name <- floaters ]
        ++[ manageDocks <+> manageHook defaultConfig
                ,(isFullscreen --> doFullFloat) --full float fullscreen flash
                ]
        where
                floaters = ["xmos2print","xcalc", "galculator", "gcalctool"]
                ignore = ["stalonetray"]
                                         
startup :: X ()
startup = return ()
