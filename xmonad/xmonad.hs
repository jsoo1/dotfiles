{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}


module Main where


import           Control.Applicative              (liftA2)
import           Control.Monad                    (join, unless)
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


main :: IO ()
main = do
  replace

  -- TODO: Fix too many bars without external display
  nScreens <- countScreens
  xmobarPipes <- traverse (xmobarCmd nScreens) [1 ..nScreens]

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
        <+> spawn "light -S 30.0"
        <+> spawn "compton --config ~/.config/compton/compton.conf"
        <+> spawn "xwallpaper --zoom ~/Downloads/richter-lucerne.jpg"
        <+> spawn "xcape -e 'Control_L=Escape'"
    }
    `additionalKeys` myCommands

-- ============== Keybindings ==============

myModMask :: KeyMask
myModMask = mod4Mask

myCommands :: [((KeyMask, KeySym), X ())]
myCommands =
  [ ( ( myModMask, xK_space ), spawn "dmenu_run -F" )
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
  , ( ( myModMask .|. shiftMask, xK_x ), spawn "xlock -mode rain" )
  , ( ( myModMask .|. shiftMask, xK_s ), spawn "loginctl suspend" )
  , ( ( myModMask, xK_Tab )
    , gotoMenuConfig $ def
      { menuCommand = "dmenu"
      , menuArgs = [ "-f", "-F", "-p", "workspace" ]
      }
    )
  , ( ( myModMask,  xK_o )
    , dmenuGitDirs >>= \dir -> unless (null dir) $ tmuxNewSession dir
    )
  , ( ( 0, xF86XK_AudioLowerVolume )
    , spawn "amixer -q set Master 2%-"
    )
  , ( ( 0, xF86XK_AudioRaiseVolume )
    , spawn "amixer -q set Master 2%+"
    )
  , ( ( 0, xF86XK_AudioMute ), spawn "amixer -q set Master toggle" )
  , ( ( 0, xF86XK_MonBrightnessUp ), spawn "light -A 2.0" )
  , ( ( 0, xF86XK_MonBrightnessDown ), spawn "light -U 2.0" )
  , ( ( myModMask, xK_n ), moveTo Next NonEmptyWS )
  , ( ( myModMask, xK_p ), moveTo Prev NonEmptyWS )
  , ( ( myModMask .|. shiftMask, xK_n ), shiftTo Next EmptyWS )
  , ( ( myModMask .|. shiftMask, xK_p ), shiftTo Prev EmptyWS )
  ]

-- ============== Dmenu/Tmux =============


tmux :: String -> X ()
tmux args =
  runInTerm "" $ "tmux " <> args


emacsCmd :: String -> String
emacsCmd args =
  "emacsclient -nw --socket-name term " <> args


dmenuGitDirs :: X String
dmenuGitDirs =
  runProcessWithInput "bash"
  [ "-c"
  , "fd '\\.git' '/' -t d -H -I \
    \-E '\\.github' \
    \-E '\\.cache' \
    \-E '\\.tmux' \
    \-E '\\.cargo' \
    \-E /gnu/store \
    \-E '\\.git-credential-cache' \
    \-E '\\.spago' \
    \| sed -E 's/\\/\\.git$//' \
    \| dmenu -f -F -p 'repository'"
  ]
  ""


tmuxNewSession :: String -> X ()
tmuxNewSession fullPath = do
  sessionName <-
    runProcessWithInput "bash"
    [ "-c"
    ,  "basename " <> fullPath
    ]
    ""
  let dotToDash c = if c == '.' then '-' else c
  let sessionName' = filter (/= '\n') $ dotToDash <$> sessionName
  let fullPath' = filter (/= '\n') fullPath
  let sessionDetails = " -c " <> fullPath' <> " -n emacs -s " <> sessionName'
  tmux $ "new-session -A" <> sessionDetails <> " ' exec " <> emacsCmd fullPath' <> "'"


-- ============== Bar ==============

xmobarCmd :: MonadIO m => Int -> Int -> m (Int, Handle)
xmobarCmd nScreens screen = do
  xmobarPipe <- spawnPipe cmd
  pure ( screen, xmobarPipe )
  where
    cmd = "xmobar ~/.config/xmobar/xmobar.hs --screen=" <> show (succ nScreens - screen)


myXmobar :: (Int, Handle) -> X ()
myXmobar (screenId, xmobarPipe) = do
  Titles {..} <- withWindowSet allTitles

  let wsPrefix :: WorkspaceId -> String
      wsPrefix wsId =
        " " ++ show screenId ++ "," ++ wsId ++ " | "

  dynamicLogWithPP $ xmobarPP
    { ppOutput =
        hPutStrLn xmobarPipe
        . (xmobarColor' base03 green (show screenId ++ ". ") ++)
    , ppCurrent =
        \wsId ->
          xmobarColor' base03 base01
          $ wsPrefix wsId ++ titleFormat current
    , ppHidden =
        \wsId ->
          xmobarAction ("xdotool key super+" ++ wsId) "1"
          $ xmobarColor' base01 base03
          $ wsPrefix wsId ++ hiddenTitle hidden wsId
    , ppVisible =
      \wsId ->
        xmobarColor' base01 base03
        $ wsPrefix wsId ++ titleFor visible wsId
    , ppUrgent =
        \wsId ->
          let
            t =
              if null $ titleFor visible wsId
              then titleFor visible wsId
              else hiddenTitle hidden wsId
          in
            xmobarAction ("xdotool key super+" ++ wsId) "1"
            $ xmobarColor' base03 base0 t
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

emptyTitle :: String
emptyTitle = "         "

titleFor :: (Show a, Show b, Ord k) => Map.Map k (b, Maybe a) -> k -> String
titleFor windowNames wsId =
  maybe emptyTitle titleFormat $  Map.lookup wsId windowNames


titleFormat :: (Show a, Show b) => (a, Maybe b) -> String
titleFormat (_, windowName) = (++ " ") $ take 20 $ maybe emptyTitle show windowName


hiddenTitle :: (Show a, Ord k) => Map.Map k (Maybe a) -> k -> String
hiddenTitle windowNames wsId =
    (++ " ") $ take 20 $ maybe emptyTitle show $ join $ Map.lookup wsId windowNames


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

green :: SpaceColor
green = SpaceColor "#859900"
