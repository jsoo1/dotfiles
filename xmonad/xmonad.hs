{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}


module Main where


import           Control.Applicative              (liftA2)
import           Control.Monad                    (join)
import           Data.Coerce                      (coerce)
import           Data.Foldable                    (traverse_)
import           Data.Function                    (on)
import           Data.List                        (intercalate)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (listToMaybe)


import           Graphics.X11.ExtraTypes.XF86
import           System.IO
import           XMonad
import           XMonad.Actions.CycleWS           (WSType (..), moveTo, shiftTo)
import           XMonad.Actions.PhysicalScreens   (getScreen)
import           XMonad.Actions.WindowBringer
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.IndependentScreens (countScreens)
import           XMonad.Layout.NoBorders          (smartBorders)
import           XMonad.Layout.Spacing
import qualified XMonad.StackSet                  as W
import           XMonad.Util.EZConfig             (additionalKeys)
import           XMonad.Util.NamedWindows
import           XMonad.Util.Replace
import           XMonad.Util.Run                  (runInTerm,
                                                   runProcessWithInput,
                                                   spawnPipe)


infixl 1 |>
(|>) :: a -> (a -> b) -> b
(|>) = flip ($)


main :: IO ()
main = do
  replace

  nScreens <- countScreens
  xmobarPipes <- traverse (spawnPipe . xmobarCmd) [0 ..nScreens]

  xmonad $ docks def
    { terminal = "alacritty"
    , focusFollowsMouse = False
    , borderWidth = 2
    , modMask = myModMask
    , normalBorderColor = coerce base03
    , focusedBorderColor = coerce base03
    , handleEventHook = handleEventHook def <+> docksEventHook
    , manageHook = manageDocks <+> manageHook def
    , layoutHook =
        avoidStruts
        $ spacingRaw True (Border 5 5 5 5) True (Border 5 5 5 5) True
        $ smartBorders
        $ layoutHook def
    , logHook = traverse_ myXmobar xmobarPipes
    , startupHook =
        spawn
          "xrandr\
          \ --output HDMI-1 --left-of eDP-1-1\
          \ --output eDP-1-1"
        <+> spawn "compton --config ~/.config/compton/compton.conf"
        <+> spawn "feh --bg-fill ~/Downloads/richter-lucerne.jpg"
    }
    `additionalKeys` myCommands

-- ============== Keybindings ==============

myModMask :: KeyMask
myModMask = mod4Mask

myCommands :: [((KeyMask, KeySym), X ())]
myCommands =
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
  -- , ( ( myModMask, xK_4 )
  --   , spawn "scrot '%Y-%m-%d_$wx$h.png' -e 'mv $f ~/screenshots/'"
  --   )
  ]


-- ============== Bar ==============

xmobarCmd :: Int -> String
xmobarCmd screen =
  "xmobar ~/.config/xmobar/xmobar.hs --screen=" <> show screen

myXmobar :: Handle -> X ()
myXmobar xmobarPipe = do
  Titles {..} <- withWindowSet allTitles

  let unScreenId :: ScreenId -> Int
      unScreenId (S i) = i
  let wsPrefix :: Maybe ScreenId -> WorkspaceId -> String
      wsPrefix screen wsId =
        " " ++ maybe " " (show . succ . unScreenId) screen ++ " | " ++ wsId ++ " "

  dynamicLogWithPP $ xmobarPP
    { ppOutput = hPutStrLn xmobarPipe
    , ppCurrent =
        \wsId ->
          xmobarColor' base03 base01
          $ wsPrefix (pure (fst current)) wsId ++ titleFormat current ++ " "
    , ppHidden =
        \wsId ->
          xmobarColor' base01 base03
          -- FIXME
          -- $ xmobarAction ("xdotool key super+" ++ wsId) "1"
          $ wsPrefix Nothing wsId ++ " " ++ hiddenTitle hidden wsId ++ " "
    , ppVisible =
      \wsId ->
        xmobarColor' base01 base03
        $ wsPrefix (fst <$> Map.lookup wsId visible) wsId
          ++ titleFor visible wsId ++ " "
    , ppUrgent =
        \wsId ->
          let
            t =
              if null $ titleFor visible wsId
              then titleFor visible wsId
              else hiddenTitle hidden wsId
          in
            xmobarColor' base03 base0 t
            -- FIXME
            -- $ xmobarAction ("xdotool key super+" ++ wsId) "1" t
    , ppSep = ""
    , ppWsSep = ""
    , ppTitle = const ""
    , ppOrder = \(ws:_:t:e) -> e ++ [ ws, t ]
    }


-- ============== Titles ==============


data WorkspaceTitles =
  Titles
    { hidden  :: Map.Map WorkspaceId (Maybe NamedWindow)
    , current :: (ScreenId, Maybe NamedWindow)
    , visible :: Map.Map WorkspaceId (ScreenId, Maybe NamedWindow)
    }


titleFor :: (Show a, Show b, Ord k) => Map.Map k (b, Maybe a) -> k -> String
titleFor windowNames wsId =
  maybe "         " titleFormat $  Map.lookup wsId windowNames


titleFormat :: (Show a, Show b) => (a, Maybe b) -> String
titleFormat (_, windowName) = take 20 $ maybe "         " show windowName


hiddenTitle :: (Show a, Ord k) => Map.Map k (Maybe a) -> k -> String
hiddenTitle windowNames wsId =
    maybe "         " show $ join $ Map.lookup wsId windowNames


allTitles :: WindowSet -> X WorkspaceTitles
allTitles W.StackSet {..} = do
  currMaster <- masterWindow (W.workspace current)
  visible' <-
    Map.fromList
    <$> traverse
        (\s -> (wsId s,) . ( W.screen s,) <$> masterWindow (W.workspace s))
        visible
  hidden' <-
    Map.fromList
    <$> traverse (\w -> (W.tag w,) <$> masterWindow w) hidden

  pure Titles
    { current = (W.screen current, currMaster)
    , visible = visible'
    , hidden = hidden'
    }

wsId :: Ord i => W.Screen i l Window sId sd -> i
wsId = W.tag . W.workspace

masterWindow :: W.Workspace i l Window -> X (Maybe NamedWindow)
masterWindow =
  traverse getName . listToMaybe . W.integrate' . W.stack


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
