{-# LANGUAGE NamedFieldPuns #-}


module Main where


import           Control.Monad                (join)
import           Data.Function                (on)
import qualified Data.Map.Strict              as Map
import           Data.Maybe                   (listToMaybe)


import           Graphics.X11.ExtraTypes.XF86
import           System.IO
import           XMonad
import           XMonad.Actions.CycleWS       (WSType(..), moveTo, shiftTo)
import           XMonad.Actions.WindowBringer
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.NoBorders      (smartBorders)
import           XMonad.Layout.Spacing
import qualified XMonad.StackSet              as W
import           XMonad.Util.EZConfig         (additionalKeys)
import           XMonad.Util.NamedWindows
import           XMonad.Util.Replace
import           XMonad.Util.Run              (spawnPipe, runProcessWithInput, runInTerm)


infixl 1 |>
(|>) :: a -> (a -> b) -> b
(|>) = flip ($)


main :: IO ()
main =
  let myModMask = mod4Mask in
  do
    replace

    xmobarPipe <- spawnPipe "xmobar ~/.config/xmobar/xmobar.hs"

    xmonad $ docks def
      { terminal = "termite"
      , focusFollowsMouse = False
      , borderWidth = 2
      , modMask = myModMask
      , normalBorderColor = unSpaceColor dkGrey
      , focusedBorderColor = unSpaceColor green
      , handleEventHook = handleEventHook def <+> docksEventHook
      , manageHook = manageDocks <+> manageHook def 
      , layoutHook =
          layoutHook def
            |> smartBorders
            |> spacingRaw True (Border 5 5 5 5) True (Border 5 5 5 5) True
            |> avoidStruts

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
                    |> xmobarActionSegment ("xdotool key super+" ++ wsId)
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
                      |> xmobarActionSegment ("xdotool key super+" ++ wsId)
            , ppSep = mempty
            , ppWsSep = mempty
            , ppTitle = const ""
            , ppOrder = \(ws:_:t:e) -> e ++ [ ws, t ]
            }

      , startupHook =
          spawn
            "xrandr\
            \ --output HDMI-1-1 --primary --left-of eDP-1-1\
            \ --output eDP-1-1"
          -- <+> spawn "compton --config ~/.config/compton/compton.conf"
          <+> spawn "feh --bg-fill ~/Downloads/richter-lucerne.jpg"
      }

        `additionalKeys`

          [ ( ( myModMask, xK_space )
            , spawn "fish -c \"rofi -show combi -modi combi\""
            )
          , ( ( myModMask .|. controlMask, xK_f)
            , broadcastMessage ToggleStruts
              <+> spawn "dbus-send \
                        \--session \
                        \--dest=org.Xmobar.Control \
                        \--type=method_call \
                        \--print-reply \
                        \'/org/Xmobar/Control' \
                        \org.Xmobar.Control.SendSignal \
                        \\"string:Toggle 0\""
              <+> refresh
            )
          , ( ( myModMask .|. shiftMask, xK_x )
            , spawn "xlock -mode rain"
            )
          , ( ( myModMask .|. shiftMask, xK_s )
            , spawn "loginctl suspend"
            )
          , ( ( myModMask, xK_Tab )
            , gotoMenuConfig $ def
              { menuCommand = "rofi"
              , menuArgs = [ "-dmenu", "-i" ]
              }
            )
          , ( ( myModMask,  xK_o )
            -- TODO: Fix TERM for tmux (should be: TERM=xterm-24bits tmux attach-session -t ...)
            , do
                selection <- runProcessWithInput "bash" ["-c", "tmux list-sessions | rofi -dmenu -i | cut -d : -f 1"] ""
                runInTerm "" ("env TERM=xterm-24bits tmux attach-session -t " <> selection)
            )
          , ( ( 0, xF86XK_AudioLowerVolume )
            , spawn "amixer -q set Master 2%-"
            )
          , ( ( 0, xF86XK_AudioRaiseVolume )
            , spawn "amixer -q set Master 2%+"
            )
          , ( ( 0, xF86XK_AudioMute )
            , spawn "amixer -q set Master toggle"
            )
          -- TODO: Figure out how to work around superuser
          , ( ( 0, xF86XK_MonBrightnessUp )
            , spawn "sudo light -A 5"
            )
          , ( ( 0, xF86XK_MonBrightnessDown )
            , spawn "sudo light -U 5"
            )
          , ( ( myModMask, xK_n )
            , moveTo Next NonEmptyWS
            )
          , ( ( myModMask, xK_p )
            , moveTo Prev NonEmptyWS
            )
          , ( ( myModMask .|. shiftMask, xK_n )
              , shiftTo Next EmptyWS
            )
          , ( ( myModMask .|. shiftMask, xK_p )
              , shiftTo Prev EmptyWS
            )
          -- TODO: Fix
          , ( ( myModMask .|. shiftMask, xK_4 )
            , spawn "scrot --select '%Y-%m-%d_$wx$h.png' -e 'mv $f ~/screenshots/'"
            )
          , ( ( myModMask, xK_4 )
            , spawn "scrot '%Y-%m-%d_$wx$h.png' -e 'mv $f ~/screenshots/'"
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


newtype SpaceColor = SpaceColor { unSpaceColor :: String }


currentWSSegmentScheme :: SegmentScheme
currentWSSegmentScheme =
  SegmentScheme
    { fgColor = base03
    , bgColorInner = base01
    , bgColorLeft = base03
    , bgColorRight = base03
    }


hiddenWSSegmentScheme :: SegmentScheme
hiddenWSSegmentScheme =
  SegmentScheme
    { fgColor = base01
    , bgColorInner =  base03
    , bgColorLeft = base03
    , bgColorRight = base03
    }


titleSegmentScheme :: SegmentScheme
titleSegmentScheme =
  SegmentScheme
    { fgColor = base01
    , bgColorInner = base03
    , bgColorLeft = base03
    , bgColorRight = base03
    }


urgentWSSegmentScheme :: SegmentScheme
urgentWSSegmentScheme =
  SegmentScheme
    { fgColor = base03
    , bgColorInner = grey
    , bgColorLeft = base03
    , bgColorRight = base03
    }


visibleWSSegmentScheme :: SegmentScheme
visibleWSSegmentScheme =
  SegmentScheme
    { fgColor = base03
    , bgColorInner = base01
    , bgColorLeft = base03
    , bgColorRight = base03
    }


xmobarSpaceColor :: SpaceColor -> SpaceColor -> String -> String
xmobarSpaceColor =
  xmobarColor `on` unSpaceColor


xmobarActionSegment :: String -> String -> String
xmobarActionSegment a c =
  "<action=`"++ a ++ "`>" ++ c ++ "</action>"


blue :: SpaceColor
blue = SpaceColor "#268bd2"


green :: SpaceColor
green = SpaceColor "#859900"


pink :: SpaceColor
pink = SpaceColor "#d33682"


cyan :: SpaceColor
cyan = SpaceColor "#2aa198"


purple :: SpaceColor
purple = SpaceColor "#6c71c4"


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

-- Solarized

base0 :: SpaceColor
base0 = SpaceColor "#839496"

base3 :: SpaceColor
base3 = SpaceColor "#fdf6e3"

base01 :: SpaceColor
base01 = SpaceColor "#586e75"

base03 :: SpaceColor
base03 = SpaceColor "#002b36"
