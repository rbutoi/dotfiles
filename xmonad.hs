import XMonad
import XMonad.Actions.UpdatePointer
import XMonad.Config.Xfce
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect

import qualified DBus as D
import qualified DBus.Client as D
import qualified XMonad.StackSet as W

modMask' = mod1Mask

myStartupHook = do
  startupHook xfceConfig
  spawn "compton -CGb"

myManageHook = composeAll
   [ title =? "Whisker Menu"  --> doFloat
   , manageDocks
   ]

main :: IO ()
main = do
  dbus <- D.connectSession
  -- Request access to the DBus name
  D.requestName dbus (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

  xmonad $ xfceConfig
    { modMask = modMask'
    , manageHook = myManageHook <+> manageHook xfceConfig
      -- no borders needed, just updatePointer
    , layoutHook = noBorders $ desktopLayoutModifiers $ reflectHoriz $ Tall 1 (3/100) (1/2) ||| Full
    , logHook    = ewmhDesktopsLogHook <+> logHook xfceConfig
      -- haskell syntax is so wat:
                   >> updatePointer (0.5, 0.5) (0, 0)
    , handleEventHook = fullscreenEventHook <+> ewmhDesktopsEventHook
    , startupHook = ewmhDesktopsStartup <+> myStartupHook
    , terminal = "xfce4-terminal"
    , borderWidth = 3
    , workspaces = map show [1..4]
    , focusedBorderColor = "#20B9A5"
    } `additionalKeysP`
    [ ("M-k", windows W.focusUp)
    , ("M-j", windows W.focusDown)
    , ("M-h", sendMessage Expand)
    , ("M-l", sendMessage Shrink)
    , ("M-S-k", windows W.swapUp)
    , ("M-S-j", windows W.swapDown)
    , ("M-<F4>", kill)
    , ("M-S-n", nextWS)
    , ("M-S-p", prevWS)
      -- for the almighty emacs
    , ("M-C-,", sendMessage (IncMasterN (-1)))
    , ("M-C-.", sendMessage (IncMasterN   1))
    , ("M-S-b", sendMessage ToggleStruts)
    , ("M-m", nextScreen)
    , ("M-S-m", windows W.focusMaster)
    , ("C-M-S-m", windows W.swapMaster)
    , ("M-S-q", spawn "xmonad --recompile && xmonad --restart")
    , ("C-M-S-q", spawn "killall xmonad-x86_64-linux && xfwm4 --daemon --replace")
    , ("M-S-<Return>"  , spawn "xfce4-terminal -e \"sh -c 'tmux a -d || tmux'\"")
    , ("M-<Return>", spawn "xfce4-terminal")
    , ("M-S-e", spawn "emacsclient -c -a emacs")
    , ("M-`", spawn "xfce4-popup-whiskermenu")
    , ("M-S-f", spawn "firefox")
    ] `removeKeysP`
    [ ("M-q"),("M-w"),("M-e"),("M-b"),("M-p"),("M-n")
    ,("M-1"),("M-2"),("M-3"),("M-4"),("M-5"),("M-6"),("M-7"),("M-8"),("M-9")
    ,("M-,"),("M-."),("M-S-/"),("M-S-r")
    ] `additionalMouseBindings`
    [ ((modMask', 9), (\_ -> prevWS))
    , ((modMask', 8), (\_ -> nextWS))
    , ((modMask', button4), (\_ -> prevWS))
    , ((modMask', button5), (\_ -> nextWS))
    ]
