import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main :: IO ()
main = do
  xmobarPipe <- spawnPipe "xmobar"

  xmonad $ def
    { terminal = "alacritty"
    , focusFollowsMouse = False
    , manageHook = manageDocks <+> manageHook def
    , layoutHook = avoidStruts $ layoutHook def
    , handleEventHook = handleEventHook def <+> docksEventHook
    , logHook = dynamicLogWithPP xmobarPP
      { ppOutput = hPutStrLn xmobarPipe
      , ppTitle = xmobarColor "green" "" . shorten 50
      }
    } `additionalKeys`
    [  ((mod1Mask, xK_space), spawn "fish -c \"rofi -show combi -modi window,windowcd,ssh,drun,run\"")
    ]
