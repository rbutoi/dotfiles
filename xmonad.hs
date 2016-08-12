import XMonad
import XMonad.Config.Xfce
import XMonad.Util.EZConfig

main = xmonad $ xfceConfig
    { modMask = mod1Mask
    , terminal    = "xfce4-terminal"
    , borderWidth = 2
    , workspaces  = map show [1..4]
    }
    `additionalKeysP`
    [ ("M-p", spawn "xfce4-popup-whiskermenu")
    ]
