import XMonad
import XMonad.Actions.PhysicalScreens
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import System.IO
import qualified XMonad.StackSet as W

myWorkspaces = [ "1", "2", "3", "4", "5", "6", "7", "8", "9" ]
keyMapping = [ ("M4-l", spawn "dm-tool lock"),
               ("M4-s", sendMessage Shrink),
               ("M4-e", sendMessage Expand),
               ("<XF86MonBrightnessUp>", spawn "xbacklight +10"),
               ("<XF86MonBrightnessDown>", spawn "xbacklight -10"),
               ("C-M-1", viewScreen 0),
               ("C-M-2", viewScreen 1)
             ] ++
             [ (otherModMasks ++ "M-" ++ [key], action tag) | (tag, key) <- zip myWorkspaces "123456789", (otherModMasks, action) <- [ ("", windows . W.view), ("S-", windows . W.shift) ] ]

manageHooks = [ manageDocks,
                isFullscreen --> doFullFloat,
                manageHook defaultConfig
              ]
myStartupHook = do
    spawn "sh ~/.fehbg"
    spawn "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --tint 0x002b36 --height 30 --alpha 0"
    spawn "nm-applet --sm-disable"
    spawn "gnome-power-manager"
    spawn "unclutter -display :0.0 -idle 1 -root -notclass google-chrome"
    spawn "pasystray"

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig {
           terminal = "konsole",
           startupHook = myStartupHook,
           workspaces = myWorkspaces,
           manageHook = composeAll manageHooks,
           layoutHook = smartBorders . avoidStruts  $  layoutHook defaultConfig,
           logHook = dynamicLogWithPP xmobarPP {
                     ppOutput = hPutStrLn xmproc,
                     ppTitle = xmobarColor "green" "" . shorten 100
                     },
           handleEventHook = fullscreenEventHook,
           modMask = mod4Mask
           } `additionalKeysP` keyMapping
