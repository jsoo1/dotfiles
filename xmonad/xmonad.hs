import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main :: IO ()
main =
  do
    xmobarPipe <- spawnPipe "/home/john/.local/bin/xmobar /home/john/.config/xmobar/xmobarrc"
    xmonad $
      docks
        def
          { terminal = "alacritty"
          , focusFollowsMouse = False
          , borderWidth = 3
          , normalBorderColor = "#292b2e" -- dark grey
          , focusedBorderColor = "#5d4d7a" -- purple
          -- "#bc6ec5" -- pink -- "#2aa1ae" -- cyan -- "#5d4d7a" -- purple -- "#f4f4f4" -- off-white

          , layoutHook =
              gaps [(U, 3)] $
                smartSpacingWithEdge 14 $
                  avoidStrutsOn [U, D] $
                    layoutHook def

          , manageHook =
              manageDocks <+> manageHook def

          , logHook =
              dynamicLogWithPP
                xmobarPP
                  { ppOutput = hPutStrLn xmobarPipe
                  , ppTitle = xmobarColor "green" "" . shorten 50
                  }

          , startupHook =
              setWMName "LG3D"
              <+> spawn "/usr/binxrandr --output HDMI-1-1 --primary --left-of eDP-1-1 --output eDP-1-1"
              <+> spawn "/usr/bin/compton --config /home/john/.config/compton/compton.conf"
              <+> spawn "/usr/bin/nitrogen --restore"
              <+> spawn "/usr/bin/setxkbmap -layout us -option ctrl:nocaps"
              <+> spawn "/usr/bin/xcape -e 'Control_L=Escape'"
          }

        `additionalKeys`
          [ ((mod1Mask, xK_space), spawn "fish -c \"rofi -show combi -modi combi\"")
          , ((mod1Mask .|. shiftMask, xK_X), spawn "sh /home/john/.i3/blurlock.sh")
          ]
