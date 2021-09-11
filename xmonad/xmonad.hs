{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}


module Main where

import           Control.Monad                    (join, unless, void, when)
import           Data.Coerce                      (coerce)
import           Data.Foldable                    (traverse_)
import           Data.Function                    (on)
import           Data.List                        (isPrefixOf)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (listToMaybe)
import qualified DBus
import qualified DBus.Client                      as DBus
import           Graphics.X11.ExtraTypes.XF86
import           System.IO
import           XMonad
import           XMonad.Actions.CycleWS           (WSType (..), moveTo, shiftTo)
import           XMonad.Actions.WindowBringer
import           XMonad.Config.Prime              (liftIO)
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.IndependentScreens (countScreens)
import           XMonad.Layout.NoBorders          (smartBorders)
import           XMonad.Layout.Spacing
import           XMonad.Layout.ThreeColumns       (ThreeCol (..))
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

  xmobarPipe <- xmobarCmd 1 1

  xmonad $ docks def
    { terminal = "alacritty"
    , focusFollowsMouse = False
    , borderWidth = 2
    , modMask = myModMask
    , normalBorderColor = coerce base03
    , focusedBorderColor = coerce base0
    , handleEventHook = handleEventHook def <+> docksEventHook
    , manageHook = manageDocks <+> manageHook def
    , layoutHook =
        avoidStruts $ gaps 5 5 $ ThreeColMid 1 (3/100) (1/2) ||| layoutHook def
    , logHook = myXmobar xmobarPipe
    , startupHook = traverse_ spawn
        [ "light -S 30.0"
        , "compton --config ~/.config/compton/compton.conf"
        , "xwallpaper --zoom ~/Downloads/richter-lucerne.jpg"
        ]
    }
    `additionalKeys` myCommands

-- ============== Layouts ==============

gaps screenGap windowGap =
  spacingRaw
      True
      (Border screenGap screenGap screenGap screenGap)
      True
      (Border windowGap windowGap windowGap windowGap)
      True
  . smartBorders

-- ============== Keybindings ==============

myModMask :: KeyMask
myModMask = mod4Mask


myCommands :: [((KeyMask, KeySym), X ())]
myCommands =
  [ ( ( myModMask, xK_i ), spawn "dmenu_run -F -p open" )
  , ( ( myModMask,  xK_o )
    , dmenuGitDirs >>= \dir -> unless (null dir) $ tmuxNewSession dir
    )
  , ( ( myModMask, xK_u ) , spawn "/home/john/.local/bin/clipmenu -p clipboard" )
  , ( ( myModMask .|. shiftMask , xK_l ) , dmenuLPass )
  , ( ( myModMask .|. controlMask, xK_f)
    , void (sendXmobar "Toggle 0") <+> broadcastMessage ToggleStruts <+> refresh
    )
  , ( ( myModMask .|. shiftMask, xK_x ), spawn "xlock -mode rain" )
  , ( ( myModMask .|. controlMask, xK_4 ), spawn "scrot" )
  , ( ( myModMask, xK_Tab )
    , gotoMenuConfig $ def
      { menuCommand = "dmenu"
      , menuArgs = [ "-f", "-F", "-p", "workspace" ]
      }
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
    \-E '/nix/store' \
    \-E '/tmp' \
    \-E '\\.local' \
    \-E 'dist-newstyle' \
    \-E '\\.opam' \
    \-E '/s3' \
    \| sed -E 's/\\/\\.git$//' \
    \| dmenu -f -F -p 'repository'"
  ]
  ""


dmenuLPass :: X ()
dmenuLPass = do
  -- login <- runProcessWithInput "bash" [ "-c", "lpass status" ] ""
  -- when ("Not logged in." `isPrefixOf` login) $ do
  --   runProcessWithInput "lpass" [ "login", "jsoo1@asu.edu" ] ""

  -- passes <- runProcessWithInput "lpass" [ "ls" , "--color=never" ] ""
  -- choice <- runProcessWithInput "bash"
  --           [ "-c", "dmenu -F -p password \
  --            \| sed -E 's/^.*id: ([0-9]+)]$/\1/'"
  --           ]
  --           passes
  -- liftIO (print choice)
  -- unless (null choice) $ void $ runProcessWithInput "bash"
  --   [ "-c"
  --   , "-i"
  --   , "lpass show --color=never " <> choice
  --     <> "| awk '/^Password:/ { printf \"%s\", $2 }' \
  --        \| xsel -ib"
  --   ]
  --   ""
  choice <- runProcessWithInput "bash" [ "-c", "lpass ls --color=never | dmenu -p password" ] ""
  liftIO (print choice)

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


xmobarMethod :: DBus.MethodCall
xmobarMethod =
  method {DBus.methodCallDestination = Just busName}
  where
    busName = DBus.busName_ "org.Xmobar.Control"
    objectPath = DBus.objectPath_ "/org/Xmobar/Control"
    interfaceName = DBus.interfaceName_ "org.Xmobar.Control"
    memberName = DBus.memberName_ "SendSignal"
    method = DBus.methodCall objectPath interfaceName memberName


sendXmobar :: String -> X DBus.MethodReturn
sendXmobar cmd = liftIO $ do
  client <- DBus.connectSession
  DBus.call_ client (xmobarMethod {DBus.methodCallBody = [DBus.toVariant cmd]})


myXmobar :: (Int, Handle) -> X ()
myXmobar (screenId, xmobarPipe) = do
  Titles {..} <- withWindowSet allTitles

  let wsPrefix :: WorkspaceId -> String
      wsPrefix wsId =
        " " ++ show screenId ++ "," ++ wsId ++ " | "

  dynamicLogWithPP $ xmobarPP
    { ppOutput =
        hPutStrLn xmobarPipe
        . (xmobarColor' base0 green (show screenId ++ ". ") ++)
    , ppCurrent =
        \wsId ->
          xmobarColor' base0 base01
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
