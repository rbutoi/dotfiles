import XMonad
import XMonad.Config.Xfce
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as W

main = xmonad $ xfceConfig
    { modMask = mod1Mask
    , terminal = "xfce4-terminal"
    , borderWidth = 4
    , workspaces = map show [1..4]
    , focusedBorderColor = "#20B9A5"
    }
    `additionalKeysP`
    [ ("M-p", spawn "xfce4-popup-whiskermenu")
    , ("M-j", windows W.focusUp)
    , ("M-k", windows W.focusDown)
    ]
