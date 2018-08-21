{-# LANGUAGE NamedFieldPuns #-}


module Main where


import           Control.Monad                (join)
import           Data.Function                (on)
import qualified Data.Map.Strict              as Map
import           Data.Maybe                   (listToMaybe)


import           Graphics.X11.ExtraTypes.XF86
import           System.IO
import           XMonad
import           XMonad.Actions.CycleWS       (WSType (..), moveTo)
import           XMonad.Actions.WindowBringer
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.NoBorders      (smartBorders)
import           XMonad.Layout.Spacing
import qualified XMonad.StackSet              as W
import           XMonad.Util.EZConfig         (additionalKeys)
import           XMonad.Util.NamedWindows
import           XMonad.Util.Replace
import           XMonad.Util.Run              (spawnPipe)


infixl 1 |>
(|>) :: a -> (a -> b) -> b
(|>) = flip ($)


main :: IO ()
main = do

    replace

    xmobarPipe <- spawnPipe "xmobar ~/.config/xmobar/xmobar.hs"

    xmonad $ docks def
      { terminal = "termite"
      , focusFollowsMouse = False
      , borderWidth = 2
      , normalBorderColor = hashCode dkGrey
      , focusedBorderColor = hashCode green
      , layoutHook =
          layoutHook def
            |> avoidStrutsOn [ U, D ]
            |> smartSpacingWithEdge 11
            |> smartBorders

      , manageHook = manageDocks <+> manageHook def <+> manageDocks

      , logHook = do
          Titles {current, hidden, visible} <- withWindowSet allTitles

          dynamicLogWithPP $ xmobarPP
            { ppOutput = hPutStrLn xmobarPipe
            , ppCurrent =
                current
                  |> maybe "       " titleFormat
                  |> wsArrowRight currentWSSegmentScheme
            , ppHidden =
                \wsId ->
                  titleFor hidden wsId
                    |> flip (wsArrowRight hiddenWSSegmentScheme) wsId
            , ppVisible =
                \wsId ->
                  titleFor visible wsId
                    |> flip (wsArrowRight visibleWSSegmentScheme) wsId
            , ppUrgent =
                \wsId ->
                  let
                    t =
                      if null $ titleFor visible wsId
                      then titleFor visible wsId
                      else titleFor hidden wsId
                  in
                    wsArrowRight urgentWSSegmentScheme t wsId
            , ppSep = mempty
            , ppWsSep = mempty
            , ppTitle =
                \t ->
                  if null t
                  then mempty
                  else titleArrowRight titleSegmentScheme t
            , ppOrder = \(ws:_:t:e) -> e ++ [ ws, t ]
            }

      , startupHook =
          spawn
            "xrandr\
            \ --output HDMI-1-1 --primary --left-of eDP-1-1\
            \ --output eDP-1-1"
          <+> spawn "compton --config ~/.config/compton/compton.conf"
          <+> spawn "feh --bg-fill ~/Downloads/richter-eisberg.jpg"
          <+> spawn "setxkbmap -layout us -option ctrl:nocaps"
      }

        `additionalKeys`

          [ ( ( mod1Mask, xK_space )
            , spawn "fish -c \"rofi -show combi -modi combi\""
            )
          , ( ( mod1Mask .|. shiftMask, xK_x )
            , spawn "xlock -mode blank"
            )
          , ( ( mod1Mask .|. shiftMask, xK_s )
            , spawn "loginctl suspend"
            )
          , ( ( mod1Mask, xK_Tab )
            , gotoMenuConfig $ def
              { menuCommand = "rofi"
              , menuArgs = [ "-dmenu", "-i" ]
              }
            )
          -- TODO: Make alsa work.
          -- , ( ( 0, xF86XK_AudioLowerVolume )
          --   , spawn "amixer -q -D pulse sset Master 2%-"
          --   )
          -- , ( ( 0, xF86XK_AudioRaiseVolume )
          --   , spawn "amixer -q -D pulse sset Master 2%+"
          --   )
          -- , ( ( 0, xF86XK_AudioMute )
          --   , spawn "amixer -q -D pulse set Master toggle"
          --   )
          , ( ( mod1Mask, xK_n )
            , moveTo Next NonEmptyWS
            )
          , ( ( mod1Mask, xK_p )
            , moveTo Prev NonEmptyWS
            )
          ]


-- ============== Titles ==============


data WorkspaceTitles =
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
  Ord i
  => (a -> [W.Workspace i l Window])
  -> a
  -> X (Map.Map i (Maybe NamedWindow))
masterWindowsFor =
  (.) (traverse masterWindow . Map.fromList . fmap (\x -> (W.tag x, x)))


currentMasterWindow :: WindowSet -> X (Maybe NamedWindow)
currentMasterWindow = masterWindow . W.workspace . W.current


wsArrowRight :: SegmentScheme -> String -> String -> String
wsArrowRight SegmentScheme { fgColor, bgColorInner, bgColorLeft, bgColorRight } wsTitle =
  xmobarSpaceColor fgColor bgColorInner
  . (xmobarSpaceColor bgColorLeft bgColorInner (show solidRightChevron) ++)
  . (++ xmobarSpaceColor bgColorInner bgColorRight (show solidRightChevron))
  . (++ xmobarSpaceColor bgColorRight bgColorInner " ")
  . (++ xmobarSpaceColor fgColor bgColorInner wsTitle)
  . (++ xmobarSpaceColor fgColor bgColorInner (show rightChevron))
  . (++ xmobarSpaceColor bgColorRight bgColorInner " ")
  . (xmobarSpaceColor bgColorRight bgColorInner " " ++)

titleArrowRight :: SegmentScheme -> String -> String
titleArrowRight SegmentScheme { fgColor, bgColorInner, bgColorLeft, bgColorRight } =
  xmobarSpaceColor fgColor bgColorInner
  . (xmobarSpaceColor bgColorLeft bgColorInner (show solidRightChevron) ++)
  . (++ xmobarSpaceColor bgColorInner bgColorRight (show solidRightChevron))
  . (++ xmobarSpaceColor bgColorRight bgColorInner " ")
  . (xmobarSpaceColor bgColorRight bgColorInner " " ++)


newtype Separator = Separator String

instance Show Separator where
  show (Separator s) = s


solidRightChevron :: Separator
solidRightChevron = Separator "\57520"


rightChevron :: Separator
rightChevron = Separator "\57521"


--  ============= COLORS ============


data SegmentScheme =
  SegmentScheme
    { fgColor      :: SpaceColor
    , bgColorInner :: SpaceColor
    , bgColorLeft  :: SpaceColor
    , bgColorRight :: SpaceColor
    }


newtype SpaceColor = SpaceColor String


currentWSSegmentScheme :: SegmentScheme
currentWSSegmentScheme =
  SegmentScheme
    { fgColor = dkGrey
    , bgColorInner = blue
    , bgColorLeft = eisbergGrey
    , bgColorRight = eisbergGrey
    }


hiddenWSSegmentScheme :: SegmentScheme
hiddenWSSegmentScheme =
  SegmentScheme
    { fgColor = blue
    , bgColorInner =  grey
    , bgColorLeft = eisbergGrey
    , bgColorRight = eisbergGrey
    }


titleSegmentScheme :: SegmentScheme
titleSegmentScheme =
  SegmentScheme
    { fgColor = dkGrey
    , bgColorInner = green
    , bgColorLeft = eisbergGrey
    , bgColorRight = eisbergGrey
    }


urgentWSSegmentScheme :: SegmentScheme
urgentWSSegmentScheme =
  SegmentScheme
    { fgColor = pink
    , bgColorInner = grey
    , bgColorLeft = eisbergGrey
    , bgColorRight = eisbergGrey
    }


visibleWSSegmentScheme :: SegmentScheme
visibleWSSegmentScheme =
  SegmentScheme
    { fgColor = blue
    , bgColorInner = grey
    , bgColorLeft = dkGrey
    , bgColorRight = dkGrey
    }


hashCode :: SpaceColor -> String
hashCode (SpaceColor hash) = hash


xmobarSpaceColor :: SpaceColor -> SpaceColor -> String -> String
xmobarSpaceColor =
  xmobarColor `on` hashCode


blue :: SpaceColor
blue = SpaceColor "#4f97d7"


green :: SpaceColor
green = SpaceColor "#2D9574"


pink :: SpaceColor
pink = SpaceColor "#bc6ec5"


cyan :: SpaceColor
cyan = SpaceColor "#2aa1ae"


purple :: SpaceColor
purple = SpaceColor "#5d4d7a"


offWhite :: SpaceColor
offWhite = SpaceColor "#f4f4f4"


grey :: SpaceColor
grey = SpaceColor "#292b2e"


dkGrey :: SpaceColor
dkGrey = SpaceColor "#15171a"


ltGrey :: SpaceColor
ltGrey = SpaceColor "grey"


eisbergGrey :: SpaceColor
eisbergGrey = SpaceColor "#778784"
