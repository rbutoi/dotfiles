import XMonad
import XMonad.Config.Xfce
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders

import qualified DBus as D
import qualified DBus.Client as D
import qualified XMonad.StackSet as W

myStartupHook = do
  startupHook xfceConfig
  spawn "compton -b"

main :: IO ()
main = do
  dbus <- D.connectSession
  -- Request access to the DBus name
  D.requestName dbus (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

  xmonad $ xfceConfig
    { modMask = mod1Mask
    , manageHook = manageDocks <+> manageHook xfceConfig
    , layoutHook = smartBorders $ layoutHook xfceConfig
    , logHook    = ewmhDesktopsLogHook <+> logHook xfceConfig
    , handleEventHook = fullscreenEventHook <+> ewmhDesktopsEventHook
    , startupHook = ewmhDesktopsStartup <+> myStartupHook
    , terminal = "xfce4-terminal"
    , borderWidth = 3
    , workspaces = map show [1..4]
    , focusedBorderColor = "#20B9A5"
    } `additionalKeysP`
    [ ("M-j", windows W.focusUp)
    , ("M-k", windows W.focusDown)
    , ("M-S-j", windows W.swapUp)
    , ("M-S-k", windows W.swapDown)
    , ("M-<F4>", kill)
    , ("M-S-n", nextWS)
    , ("M-S-p", prevWS)
    , ("M-C-,", sendMessage (IncMasterN 1)) -- for the almighty emacs
    , ("M-C-.", sendMessage (IncMasterN (-1)))
    -- xfce
    -- , ("M-S-e", spawn "emacsclient -c -a emacs")
    , ("M-S-b", sendMessage ToggleStruts)
    , ("M-S-m", windows W.swapMaster)
    -- let xfce handle it, so I can use it with xfwm4 as well
    -- , ("M-C-<Return>", spawn "xfce4-terminal -e \"sh -c 'tmux a -d || tmux'\"")
    , ("M-S-q", spawn "xmonad --recompile && xmonad --restart")
    , ("C-M-S-q", spawn "killall xmonad-x86_64-linux && xfwm4 --daemon --replace")
    ] `removeKeysP`
    [ ("M-q"),("M-w"),("M-e"),("M-b"),("M-p"),("M-n")
    ,("M-1"),("M-2"),("M-3"),("M-4"),("M-5"),("M-6"),("M-7"),("M-8"),("M-9")
    ,("M-<Return>"),("M-,"),("M-.")
    ]
