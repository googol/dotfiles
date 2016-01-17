import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Layout.NoBorders
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import System.IO

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
           } `additionalKeysP`
           [ ("M4-l", spawn "gnome-screensaver-command -l"),
             ("M4-s", sendMessage Shrink),
             ("M4-e", sendMessage Expand)
           ]
