import XMonad
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.WindowBringer
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.Cursor
import System.IO
import qualified XMonad.StackSet as W

myWorkspaces = [ "1", "2", "3", "4", "5", "6", "7", "8", "9" ]
keyMapping = [ ("M4-l", spawn "dm-tool lock")
             , ("M4-s", sendMessage Shrink)
             , ("M4-e", sendMessage Expand)
             , ("<XF86MonBrightnessUp>", spawn "light -A 10")
             , ("<XF86MonBrightnessDown>", spawn "light -U 10")
             , ("<XF86AudioRaiseVolume>", spawn "pamixer --increase 5")
             , ("<XF86AudioLowerVolume>", spawn "pamixer --decrease 5")
             , ("<XF86AudioMute>", spawn "pamixer --toggle-mute")
             , ("C-M-1", viewScreen 0)
             , ("C-M-2", viewScreen 1)
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
    spawn "gnome-power-manager"
    spawn "unclutter -display :0.0 -idle 1 -root -notclass google-chrome"
    spawn "pasystray"

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ docks $ def {
           terminal = "konsole",
           startupHook = setDefaultCursor xC_left_ptr <+> startUpScripts,
           workspaces = myWorkspaces,
           manageHook = myManageHook,
           layoutHook = smartBorders . avoidStruts  $  layoutHook def,
           logHook = dynamicLogWithPP xmobarPP {
                     ppOutput = hPutStrLn xmproc,
                     ppTitle = xmobarColor "green" "" . shorten 100
                     },
           handleEventHook = fullscreenEventHook,
           modMask = mod4Mask
           } `additionalKeysP` keyMapping

