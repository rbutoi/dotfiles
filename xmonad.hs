import XMonad
import XMonad.Config.Xfce
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops

import qualified XMonad.StackSet as W

main = xmonad $ xfceConfig
    { modMask = mod1Mask
    , handleEventHook = fullscreenEventHook
    , terminal = "xfce4-terminal"
    , borderWidth = 3
    , workspaces = map show [1..4]
    , focusedBorderColor = "#20B9A5"
    } `additionalKeysP`
    [ ("M-j", windows W.focusUp)
    , ("M-k", windows W.focusDown)
    , ("M-S-j", windows W.swapUp)
    , ("M-S-k", windows W.swapDown)
    , ("M-S-n", nextWS)
    , ("M-S-p", prevWS)
    , ("M-C-,", sendMessage (IncMasterN 1)) -- for the almighty emacs
    , ("M-C-.", sendMessage (IncMasterN (-1)))
    , ("M-S-e", spawn "emacsclient -c -a emacs")
    , ("M-S-b", sendMessage ToggleStruts)
    , ("M-S-m", windows W.swapMaster)
    , ("M-S-h", spawn "/home/radu/.local/bin/hidpi_mode.sh")
    , ("M-C-<Return>", spawn "xfce4-terminal -e \"sh -c 'tmux a -d || tmux'\"")
    , ("M-S-q", spawn "xmonad --recompile && xmonad --restart")
    ] `removeKeysP`
    [ ("M-q"),("M-w"),("M-e"),("M-b"),("M-p"),("M-n")
    ,("M-1"),("M-2"),("M-3"),("M-4"),("M-5"),("M-6"),("M-7"),("M-8"),("M-9")
    ,("M-<Return>"),("M-,"),("M-.")
    ]
