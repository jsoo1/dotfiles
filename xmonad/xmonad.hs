module Main where


import           System.IO
import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.NoBorders  (smartBorders)
import           XMonad.Layout.Spacing
import           XMonad.Util.EZConfig     (additionalKeys, removeKeys)
import           XMonad.Util.Run          (spawnPipe)


main :: IO ()
main =
  do
    xmobarPipe <- spawnPipe "/home/john/.local/bin/xmobar /home/john/.config/xmobar/xmobar.hs"
    xmonad $
      docks
        def
          { terminal = "alacritty"
          , focusFollowsMouse = False
          , borderWidth = 3
          , normalBorderColor = "#292b2e" -- dark grey
          , focusedBorderColor = "#5d4d7a" -- purple
          -- "#2D9574" -- green -- "#bc6ec5" -- pink -- "#2aa1ae" -- cyan -- "#5d4d7a" -- purple -- "#f4f4f4" -- off-white

          , layoutHook =
              smartBorders $
                smartSpacingWithEdge 13 $
                  avoidStrutsOn [U, D] $
                    layoutHook def

          , manageHook = manageDocks <+> manageHook def

          , logHook =
              dynamicLogWithPP
                xmobarPP
                  { ppOutput = hPutStrLn xmobarPipe
                  , ppCurrent =
                      myWsTemplate "grey" "#5d4d7a" "#15171a" "#15171a"
                  , ppHidden =
                      myWsTemplate "grey" "" "#15171a" "#15171a"
                  , ppUrgent =
                      myWsTemplate "#ce537a" "#15171a" "#15171a" "#15171a"
                  , ppSep = ""
                  , ppWsSep = ""
                  , ppTitle =
                      myWsTemplate "#15171a" "#2D9574" "#15171a" "#15171a"
                      . shorten 30
                  , ppLayout = const ""
                  }

          , startupHook =
              setWMName "LG3D"
              <+> spawn "/usr/bin/xrandr --output HDMI-1-1 --primary --left-of eDP-1-1 --output eDP-1-1"
              <+> spawn "/usr/bin/compton --config /home/john/.config/compton/compton.conf"
              <+> spawn "/usr/bin/nitrogen --restore"
              <+> spawn "/usr/bin/setxkbmap -layout us -option ctrl:nocaps"
              <+> spawn "/usr/bin/xcape -e \'Control_L=Escape\'"
          }

        `additionalKeys`
          [ ((mod1Mask, xK_space), spawn "fish -c \"rofi -show combi -modi combi\"")
          , ((mod1Mask .|. shiftMask, xK_x), spawn "sh /home/john/.i3/blurlock.sh")
          ]

        `removeKeys`
          [ (mod1Mask, xK_n)
          , (mod1Mask, xK_p)
          ]


myWsTemplate :: String -> String -> String -> String -> (String -> String)
myWsTemplate fgColor bgColorInner bgColorLeft bgColorRight =
  xmobarColor fgColor bgColorInner
  . (xmobarColor bgColorLeft bgColorInner "\57520" ++)
  . (xmobarColor fgColor bgColorInner "\57521 " ++)
  . (++ xmobarColor bgColorInner bgColorRight "\57520")
  . (++ xmobarColor bgColorRight bgColorInner " " )
