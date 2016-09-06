import XMonad
import XMonad.Config.Xfce
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet as W

main = xmonad $ xfceConfig
    { modMask = mod1Mask
    , terminal = "xfce4-terminal"
    , borderWidth = 5
    , workspaces = map show [1..4]
    , focusedBorderColor = "#20B9A5"
    } `additionalKeysP`
    [ ("M-`", spawn "xfce4-popup-whiskermenu")
    , ("M-j", windows W.focusUp)
    , ("M-k", windows W.focusDown)
    , ("M-S-n", nextWS)
    , ("M-S-p", prevWS)
    , ("M-S-b", sendMessage ToggleStruts)
    , ("M-S-m", windows W.swapMaster)
    , ("M-S-q", spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad)
    ] `removeKeysP`
    [ ("M-q")
    , ("M-w")
    , ("M-e")
    , ("M-b")
    , ("M-p")
    , ("M-n")
    , ("M-1")
    , ("M-2")
    , ("M-3")
    , ("M-4")
    , ("M-5")
    , ("M-6")
    , ("M-7")
    , ("M-8")
    , ("M-9")
    , ("M-<Return>")
    ]
