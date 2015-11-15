import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Layout.NoBorders
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
    spawn "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --tint 0x191970 --height 15"
    spawn "nm-applet --sm-disable"
    spawn "gnome-power-manager"
    spawn "gpg-agent --enable-ssh-support --daemon"
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
           } `additionalKeys`
           [ ((mod4Mask, xK_l), spawn "gnome-screensaver-command -l"),
             ((mod4Mask, xK_s), sendMessage Shrink),
             ((mod4Mask, xK_e), sendMessage Expand)
           ]
