{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}


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
      , normalBorderColor = unSpaceColor base03
      , focusedBorderColor = unSpaceColor base03
      , handleEventHook = handleEventHook def <+> docksEventHook
      , manageHook = manageDocks <+> manageHook def
      , layoutHook =
          avoidStruts
          $ spacingRaw True (Border 5 5 5 5) True (Border 5 5 5 5) True
          $ smartBorders
          $ layoutHook def

      , logHook = do
          Titles {..} <- withWindowSet allTitles

          dynamicLogWithPP $ xmobarPP
            { ppOutput = hPutStrLn xmobarPipe
            , ppCurrent =
                \wsId ->
                  xmobarColor' base03 base01
                  $ " " ++ wsId ++ " " ++ maybe "      " titleFormat current ++ " "
            , ppHidden =
                \wsId ->
                  xmobarColor' base01 base03
                  -- FIXME
                  -- $ xmobarAction ("xdotool key super+" ++ wsId) "1" 
                  $ " " ++ wsId ++ " " ++ titleFor hidden wsId ++ " "
            , ppVisible =
              \wsId ->
                xmobarColor' base01 base03
                $ " " ++ wsId ++ " " ++ titleFor visible wsId ++ " "
            , ppUrgent =
                \wsId ->
                  let
                    t =
                      if null $ titleFor visible wsId
                      then titleFor visible wsId
                      else titleFor hidden wsId
                  in
                    xmobarColor' base03 base0 t
                    -- FIXME
                    -- $ xmobarAction ("xdotool key super+" ++ wsId) "1" t
            , ppSep = ""
            , ppWsSep = ""
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
          -- , ( ( myModMask .|. shiftMask, xK_4 )
          --   , spawn "scrot --select '%Y-%m-%d_$wx$h.png' -e 'mv $f ~/screenshots/'"
          --   )
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
  maybe "" titleFormat $ join $ Map.lookup wsId windowNames


titleFormat :: Show a => a -> String
titleFormat =
  take 8 . (" " ++) . show


allTitles :: WindowSet -> X WorkspaceTitles
allTitles windowSet = do
  current <- currentMasterWindow windowSet
  visible <- masterWindowsFor (fmap W.workspace . W.visible) windowSet
  hidden <- masterWindowsFor W.hidden windowSet

  pure Titles {..}


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


--  ============= COLORS ============

newtype SpaceColor = SpaceColor { unSpaceColor :: String }

xmobarColor' :: SpaceColor -> SpaceColor -> String -> String
xmobarColor' =
  xmobarColor `on` unSpaceColor

-- Solarized

base0 :: SpaceColor
base0 = SpaceColor "#839496"

base3 :: SpaceColor
base3 = SpaceColor "#fdf6e3"

base01 :: SpaceColor
base01 = SpaceColor "#586e75"

base03 :: SpaceColor
base03 = SpaceColor "#002b36"
