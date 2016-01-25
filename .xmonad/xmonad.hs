import XMonad
import XMonad.Actions.PhysicalScreens
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Layout.NoBorders
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import System.IO
import qualified XMonad.StackSet as W

myWorkspaces = [ "1", "2", "3", "4", "5", "6", "7", "8", "9" ]
keyMapping = [ ("M4-l", spawn "gnome-screensaver-command -l"),
               ("M4-s", sendMessage Shrink),
               ("M4-e", sendMessage Expand),
               ("<XF86MonBrightnessUp>", spawn "xbacklight +10"),
               ("<XF86MonBrightnessDown>", spawn "xbacklight -10"),
               ("C-M-1", viewScreen 0),
               ("C-M-2", viewScreen 1)
             ] ++
             [ (otherModMasks ++ "M-" ++ [key], action tag)
               | (tag, key) <- zip myWorkspaces "123456789"
               , (otherModMasks, action) <- [ ("", windows . W.view)
                                              , ("S-", windows . W.shift) ]
             ]

main = do
    spawn "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --tint 0x191970 --height 15"
    spawn "nm-applet --sm-disable"
    spawn "gnome-power-manager"
    spawn "gpg-agent --enable-ssh-support --daemon --write-env-file /home/miika/.gpg-agent-info"
    spawn "unclutter -display :0.0 -idle 1 -root -notclass google-chrome"
    spawn "megasync"
    spawn "pasystray"
    spawn "xflux -l 60.198057 -g 24.951908 -nofork"
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig {
           workspaces = myWorkspaces,
           manageHook = composeAll [ manageDocks,
                                     isFullscreen --> doFullFloat,
                                     manageHook defaultConfig
                                   ],
           layoutHook = smartBorders . avoidStruts  $  layoutHook defaultConfig,
           logHook = dynamicLogWithPP xmobarPP {
                     ppOutput = hPutStrLn xmproc,
                     ppTitle = xmobarColor "green" "" . shorten 100
                     },
           modMask = mod4Mask
           } `additionalKeysP` keyMapping
