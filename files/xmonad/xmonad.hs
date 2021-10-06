{-# LANGUAGE NamedFieldPuns #-}
import XMonad
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.WindowBringer
import XMonad.Core (runQuery)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.Cursor
import System.IO
import Data.Maybe (maybeToList)
import Data.Monoid (All(..))
import Control.Monad (when, join)
import qualified XMonad.StackSet as W
import Graphics.X11.Xlib.Extras (Event(..))

myWorkspaces = [ "1", "2", "3", "4", "5", "6", "7", "8", "9" ]
keyMapping = [ ("M4-l", spawn "slock")
             , ("M4-s", sendMessage Shrink)
             , ("M4-e", sendMessage Expand)
             , ("<XF86MonBrightnessUp>", spawn "sudo light -A 9")
             , ("<XF86MonBrightnessDown>", spawn "sudo light -U 9")
             , ("<XF86AudioRaiseVolume>", spawn "pamixer --increase 5")
             , ("<XF86AudioLowerVolume>", spawn "pamixer --decrease 5")
             , ("<XF86AudioMute>", spawn "pamixer --toggle-mute")
             -- , ("C-M-1", viewScreen 0)
             -- , ("C-M-2", viewScreen 1)
             , ("M-b", gotoMenu)
             ] ++
             [ (otherModMasks ++ "M-" ++ [key], action tag) | (tag, key) <- zip myWorkspaces "123456789", (otherModMasks, action) <- [ ("", windows . W.view), ("S-", windows . W.shift) ] ]

myManageHook = composeAll
    [ isFullscreen --> doFullFloat
    , manageHook def
    ]

startUpScripts = do
    spawn "sh ~/.fehbg"
    spawn "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --tint 0x002b36 --height 30 --alpha 0"
    spawn "nm-applet --sm-disable"
    spawn "unclutter -display :0.0 -idle 1 -root"
    spawn "blueman-applet"
    spawn "pasystray"
    spawn "twmnd"
    spawn "caffeine"

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ docks $ ewmh $ def {
           terminal = "konsole",
           startupHook = setDefaultCursor xC_left_ptr <+> startUpScripts,
           workspaces = myWorkspaces,
           manageHook = myManageHook,
           layoutHook = smartBorders . avoidStruts  $  layoutHook def,
           logHook = dynamicLogWithPP xmobarPP {
                     ppOutput = hPutStrLn xmproc,
                     ppTitle = xmobarColor "green" "" . shorten 100
                     },
           handleEventHook = handleEventHook def <+> ewmhDesktopsEventHook <+> fullscreenEventHook,
           modMask = mod4Mask
           } `additionalKeysP` keyMapping

