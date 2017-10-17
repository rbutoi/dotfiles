import XMonad
import XMonad.Config.Xfce
-- import XMonad.Config.Kde
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageDocks

import qualified XMonad.StackSet as W

main = xmonad $ xfceConfig
-- main = xmonad $ kdeConfig
    { modMask = mod1Mask
    , terminal = "xfce4-terminal"
    -- , terminal = "konsole"
    , borderWidth = 3
    , workspaces = map show [1..4]
    , focusedBorderColor = "#20B9A5"
    } `additionalKeysP`
    [
      ("M-`", spawn "xfce4-popup-whiskermenu")
    , ("M-j", windows W.focusUp)
    , ("M-k", windows W.focusDown)
    , ("M-S-j", windows W.swapUp)
    , ("M-S-k", windows W.swapDown)
    , ("M-S-n", nextWS)
    , ("M-S-p", prevWS)
    , ("M-S-e", spawn "emacsclient -c -a emacs")
    , ("M-S-b", sendMessage ToggleStruts)
    , ("M-S-m", windows W.swapMaster)
    , ("M-S-h", spawn "/home/radu/.local/bin/hidpi_mode.sh")
    , ("M-S-q", spawn "xmonad --recompile && xmonad --restart") -- %! Restart xmonad)
    ] `removeKeysP`
    [ ("M-q"),("M-w"),("M-e"),("M-b"),("M-p"),("M-n"),("M-1"),("M-2"),("M-3"),("M-4"),("M-5"),("M-6"),("M-7"),("M-8"),("M-9"),("M-<Return>")
    ]
