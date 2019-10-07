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
import           XMonad.Actions.CycleWS       (WSType (..), moveTo, shiftTo)
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
          Titles {current, hidden, visible} <- withWindowSet allTitles

          dynamicLogWithPP $ xmobarPP
            { ppOutput = hPutStrLn xmobarPipe
            , ppCurrent =
              current
              |> maybe "       " titleFormat
              |> wsArrowRight dkGrey blue dkGrey dkGrey
            , ppHidden =
              \wsId ->
                titleFor hidden wsId
                |> flip (wsArrowRight blue grey dkGrey dkGrey) wsId
            , ppVisible =
              \wsId ->
                titleFor visible wsId
                |> flip (wsArrowRight blue grey dkGrey dkGrey) wsId
            , ppUrgent =
              \wsId ->
                let
                  t =
                    if null $ titleFor visible wsId
                    then titleFor visible wsId
                    else titleFor hidden wsId
                in
                  wsArrowRight pink grey dkGrey dkGrey t wsId
            , ppSep = mempty
            , ppWsSep = mempty
            , ppTitle =
              \t ->
                if null t
                then mempty
                else titleArrowRight dkGrey green dkGrey dkGrey t
            , ppOrder = \(ws:_:t:e) -> e ++ [ ws, t ]
            }

      , startupHook =
        -- setWMName "LG3D"
        spawn
          "xrandr\
          \ --output HDMI-1-1 --primary --left-of eDP-1-1\
          \ --output eDP-1-1"
        <+> spawn
          "compton --config /home/john/.config/compton/compton.conf"
        <+> spawn
          "feh --bg-fill /home/john/Downloads/richter-lake-lucerne.jpg"
        <+> spawn
          "setxkbmap -layout us -option ctrl:nocaps"
        -- <+> spawn
        --   "xcape -e \'Control_L=Escape\'"
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
          , ( (mod1Mask, xK_p)
            , moveTo Prev NonEmptyWS
            )
          , ( (mod1Mask, xK_n)
            , moveTo Next NonEmptyWS
            )
          , ( (mod1Mask .|. shiftMask, xK_p)
            , shiftTo Prev EmptyWS
            )
          , ( (mod1Mask .|. shiftMask, xK_n)
            , shiftTo Next EmptyWS
            )
          , ( (mod1Mask, xF86XK_AudioLowerVolume)
            , spawn "amixer -q -D pulse sset Master 2%-"
            )
          , ( (mod1Mask, xF86XK_AudioRaiseVolume)
            , spawn "amixer -q -D pulse sset Master 2%+"
            )
          , ( (mod1Mask, xF86XK_AudioMute)
              , spawn "amixer -q -D pulse set Master toggle"
            )
          ]


tata WorkspaceTitles =
  Titles
  { hidden  :: Map.Map WorkspaceId (Maybe NamedWindow)
  , current :: Maybe NamedWindow
  , visible :: Map.Map WorkspaceId (Maybe NamedWindow)
  }


titleFor :: (Show a, Ord k) => Map.Map k (Maybe a) -> k -> String
titleFor windowNames wsId =
  Map.lookup wsId windowNames
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
currentMasterWindow = masterWindow . W.workspace . W.current


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

titleArrowRight ::
  SpaceColor ->
  SpaceColor ->
  SpaceColor ->
  SpaceColor ->
  String ->
  String
titleArrowRight fgColor bgColorInner bgColorLeft bgColorRight =
  xmobarColor fgColor bgColorInner
  . (xmobarColor bgColorLeft bgColorInner solidRightChevron ++)
  . (++ xmobarColor bgColorInner bgColorRight solidRightChevron)
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
