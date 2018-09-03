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
keyMapping = [ ("M4-l", spawn "dm-tool lock")
             , ("M4-s", sendMessage Shrink)
             , ("M4-e", sendMessage Expand)
             , ("<XF86MonBrightnessUp>", spawn "light -A 10")
             , ("<XF86MonBrightnessDown>", spawn "light -U 10")
             , ("<XF86AudioRaiseVolume>", spawn "pamixer --increase 5")
             , ("<XF86AudioLowerVolume>", spawn "pamixer --decrease 5")
             , ("<XF86AudioMute>", spawn "pamixer --toggle-mute")
             -- , ("C-M-1", viewScreen 0)
             -- , ("C-M-2", viewScreen 1)
             , ("M-b", gotoMenu)
             ] ++
             [ (otherModMasks ++ "M-" ++ [key], action tag) | (tag, key) <- zip myWorkspaces "123456789", (otherModMasks, action) <- [ ("", windows . W.view), ("S-", windows . W.shift) ] ]

wmNormalHintsAtom :: Atom
wmNormalHintsAtom = 0x28

firefoxFullFloatEventHook :: Event -> X All
firefoxFullFloatEventHook ev@(PropertyEvent { ev_event_display, ev_atom, ev_window })
    | (ev_atom == wmNormalHintsAtom) = do
        ff <- runQuery isFirefox ev_window
        fs <- isFirefoxFullScreen ev_event_display ev_window
        when (ff && fs) $ runQuery doFullFloat ev_window >> return ()
        return mempty
firefoxFullFloatEventHook _ = return mempty

isFirefox :: Query Bool
isFirefox = className =? "Firefox"

isFirefoxFullScreen :: Display -> Window -> X Bool
isFirefoxFullScreen disp win = do
    sizeHints <- liftIO $ getWMNormalHints disp win
    case sh_base_size sizeHints of
        Just (width, height) -> return (height == 0 || width == 0)
        Nothing -> return False

myManageHook = composeAll
    [ isFullscreen --> doFullFloat
    , manageHook def
    ]

startUpScripts = do
    spawn "sh ~/.fehbg"
    spawn "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --tint 0x002b36 --height 30 --alpha 0"
    spawn "nm-applet --sm-disable"
    spawn "unclutter -display :0.0 -idle 1 -root -notclass google-chrome"
    spawn "blueman-applet"
    spawn "pasystray"
    addEWMHFullscreen

addEWMHFullscreen :: X ()
addEWMHFullscreen = do
    supported <- mapM getAtom [ "_NET_WM_STATE"
                              , "_NET_WM_STATE_FULLSCREEN"
                              , "_NET_WM_STATE_HIDDEN"
                              , "_NET_NUMBER_OF_DESKTOPS"
                              , "_NET_CLIENT_LIST"
                              , "_NET_CLIENT_LIST_STACKING"
                              , "_NET_CURRENT_DESKTOP"
                              , "_NET_DESKTOP_NAMES"
                              , "_NET_ACTIVE_WINDOW"
                              , "_NET_WM_DESKTOP"
                              , "_NET_WM_STRUT"
                              ]
    addNETSupported supported

addNETSupported :: [Atom] -> X ()
addNETSupported x = withDisplay $ \dpy -> do
    r <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a <- getAtom "ATOM"
    liftIO $ changeProperty32 dpy r a_NET_SUPPORTED a propModeReplace (fmap fromIntegral x)
    -- liftIO $ do
    --     sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
    --     when (fromIntegral x `notElem` sup) $
    --         changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

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
           handleEventHook = firefoxFullFloatEventHook <+> fullscreenEventHook,
           modMask = mod4Mask
           } `additionalKeysP` keyMapping

