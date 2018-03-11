{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}


module Main where


import           Control.Monad                (join)
import qualified Data.Map.Strict              as Map
import           Data.Maybe                   (listToMaybe)


import           Graphics.X11.ExtraTypes.XF86
import           System.IO
import           XMonad
import           XMonad.Actions.WindowBringer
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.NoBorders      (smartBorders)
import           XMonad.Layout.Spacing
import qualified XMonad.StackSet              as W
import           XMonad.Util.EZConfig         (additionalKeys, removeKeys)
import           XMonad.Util.NamedWindows
import           XMonad.Util.Replace
import           XMonad.Util.Run              (spawnPipe)


infixl 1 |>
(|>) :: a -> (a -> b) -> b
(|>) = flip ($)


main :: IO ()
main = do

    replace

    xmobarPipe <- spawnPipe "/home/john/.local/bin/xmobar\
                            \ /home/john/.config/xmobar/xmobar.hs"

    xmonad $ docks def
      { terminal = "alacritty"
      , focusFollowsMouse = False
      , borderWidth = 4
      , normalBorderColor = dkGrey
      , focusedBorderColor = green
      , layoutHook =

        layoutHook def
        |> avoidStrutsOn [ U, D ]
        |> smartSpacingWithEdge 13
        |> smartBorders

      , manageHook = manageDocks <+> manageHook def <+> manageDocks

      , logHook = do
          Titles {current, hidden} <- withWindowSet allTitles

          dynamicLogWithPP $ xmobarPP
            { ppOutput = hPutStrLn xmobarPipe
              , ppCurrent =
                current
                |> maybe "" titleFormat
                |> wsArrowRight ltGrey purple dkGrey dkGrey
              , ppHidden =
                \wsId ->
                  wsId
                  |> wsArrowRight ltGrey grey dkGrey dkGrey (titleFor hidden wsId)
              , ppUrgent = wsArrowRight pink grey dkGrey dkGrey ""
              , ppSep = ""
              , ppWsSep = ""
              , ppTitle = wsArrowRight dkGrey green dkGrey dkGrey ""
              , ppOrder = \(ws:_:t:e) -> [ ws, t ] ++ e
              }

      , startupHook =
        -- setWMName "LG3D"
        spawn
          "/usr/bin/xrandr\
          \ --output HDMI-1-1 --primary --left-of eDP-1-1\
          \ --output eDP-1-1"
        <+> spawn
          "/usr/bin/compton --config /home/john/.config/compton/compton.conf"
        <+> spawn
          "/usr/bin/nitrogen --restore"
        <+> spawn
          "/usr/bin/setxkbmap -layout us -option ctrl:nocaps"
        <+> spawn
          "/usr/bin/xcape -e 'Control_L=Escape'"
      }

        `additionalKeys`

          [ ( (mod1Mask, xK_space)
            , spawn "fish -c \"rofi -show combi -modi combi\""
            )
          , ( (mod1Mask .|. shiftMask, xK_x)
              , spawn "sh /home/john/.i3/blurlock.sh"
            )
          , ( (mod1Mask, xK_g)
            , gotoMenuConfig $ def
              { menuCommand = "/usr/bin/rofi"
              , menuArgs = [ "-dmenu", "-i" ]
              }
            )
          , ( (mod1Mask, xK_b)
            , bringMenuConfig $ def
              { menuCommand = "/usr/bin/rofi"
              , menuArgs = [ "-dmenu", "-i" ]
              }
            )
          , ( (0, xF86XK_AudioLowerVolume)
            , spawn "amixer -q -D pulse sset Master 2%-"
            )
          , ( (0, xF86XK_AudioRaiseVolume)
            , spawn "amixer -q -D puls sset Master 2%+"
            )
          , ( (0, xF86XK_AudioMute)
              , spawn "amixer -q -D pulse set Master toggle"
            )
          ]

        `removeKeys`
          [ (mod1Mask, xK_n)
          , (mod1Mask, xK_p)
          ]


data WorkspaceTitles =
  Titles
  { hidden  :: Map.Map WorkspaceId (Maybe NamedWindow)
  , current :: Maybe NamedWindow
  , visible :: Map.Map WorkspaceId (Maybe NamedWindow)
  }


titleFor :: (Show a, Ord k) => Map.Map k (Maybe a) -> k -> String
titleFor hidden wsId =
  Map.lookup wsId hidden
  |> join
  |> maybe "" titleFormat


titleFormat :: Show a => a -> String
titleFormat =
  take 7 . (" " ++) . show


allTitles :: WindowSet -> X WorkspaceTitles
allTitles windowSet = do
  currentTitle <- currentMasterWindow windowSet
  visibleTitles <- masterWindowsFor (fmap W.workspace . W.visible) windowSet
  hiddenTitles <- masterWindowsFor W.hidden windowSet

  pure Titles
    { current = currentTitle
    , visible = visibleTitles
    , hidden = hiddenTitles
    }


masterWindow :: W.Workspace i l Window -> X (Maybe NamedWindow)
masterWindow =
  traverse getName . listToMaybe . W.integrate' . W.stack


masterWindowsFor ::
  Ord i =>
  (a -> [W.Workspace i l Window]) ->
  a ->
  X (Map.Map i (Maybe NamedWindow))
masterWindowsFor =
  (.) (traverse masterWindow . Map.fromList . fmap (\x -> (W.tag x, x)))


currentMasterWindow :: WindowSet -> X (Maybe NamedWindow)
currentMasterWindow =
  masterWindow
  . W.workspace
  . W.current


wsArrowRight ::
  SpaceColor ->
  SpaceColor ->
  SpaceColor ->
  SpaceColor ->
  String ->
  String ->
  String
wsArrowRight fgColor bgColorInner bgColorLeft bgColorRight wsTitle =
  xmobarColor fgColor bgColorInner
  . (xmobarColor bgColorLeft bgColorInner solidRightChevron ++)
  . (++ xmobarColor bgColorInner bgColorRight solidRightChevron)
  . (++ xmobarColor bgColorRight bgColorInner " ")
  . (++ xmobarColor fgColor bgColorInner wsTitle)
  . (++ xmobarColor fgColor bgColorInner rightChevron)
  . (++ xmobarColor bgColorRight bgColorInner " ")
  . (xmobarColor bgColorRight bgColorInner " " ++)


type Separator = String


solidRightChevron :: Separator
solidRightChevron = "\57520"


rightChevron :: Separator
rightChevron = "\57521"


type SpaceColor = String


blue :: SpaceColor
blue = "#4f97d7"


green :: SpaceColor
green = "#2D9574"


pink :: SpaceColor
pink = "#bc6ec5"


cyan :: SpaceColor
cyan = "#2aa1ae"


purple :: SpaceColor
purple = "#5d4d7a"


offWhite :: SpaceColor
offWhite = "#f4f4f4"


grey :: SpaceColor
grey = "#292b2e"


dkGrey :: SpaceColor
dkGrey = "#15171a"


ltGrey :: SpaceColor
ltGrey = "grey"
