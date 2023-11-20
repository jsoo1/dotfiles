{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
{-# OPTIONS_GHC -Werror -Wincomplete-record-updates
                        -Wmonomorphism-restriction
                        -Wincomplete-uni-patterns
                        -Wmissing-deriving-strategies
                        -Wno-error=deprecations #-}

module Main where

import           Control.Concurrent               (ThreadId, forkIO)
import qualified Control.Concurrent.STM           as STM
import           Control.Monad                    (join, unless, void, when)
import           Data.Coerce                      (coerce)
import           Data.Foldable                    (traverse_)
import           Data.Function                    (on)
import           Data.List                        (intersperse, isPrefixOf)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (listToMaybe)
import           Graphics.X11.ExtraTypes.XF86
import           System.IO
import           System.Process                   (createPipe)
import           XMonad
import           XMonad.Actions.CycleWS           (WSType (..), moveTo, shiftTo)
import           XMonad.Actions.WindowBringer
import           XMonad.Config.Prime              (liftIO)
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.IndependentScreens (countScreens)
import           XMonad.Layout.LayoutModifier     (ModifiedLayout)
import           XMonad.Layout.NoBorders          (SmartBorder, smartBorders)
import           XMonad.Layout.Spacing
import           XMonad.Layout.ThreeColumns       (ThreeCol (..))
import qualified XMonad.StackSet                  as W
import           XMonad.Util.EZConfig             (additionalKeys)
import           XMonad.Util.NamedWindows
import           XMonad.Util.Replace
import           XMonad.Util.Run                  (runInTerm,
                                                   runProcessWithInput,
                                                   spawnPipe)
import qualified Xmobar
import qualified Xmobar.Config.Actions            as Action
import qualified Xmobar.Config.Template.Parse     as Template


main :: IO ()
main = do
  dirs <- getDirectories

  replace
  (xmobarQueue, xmobarSignal, xmobarProc) <- startXmobar

  let cfg = docks def
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
       , logHook = myXmobarPP xmobarQueue
       , startupHook = traverse_ spawn
           [ "light -S 30.0"
           , "compton --config ~/.config/compton/compton.conf"
           , "xwallpaper --zoom ~/Downloads/richter-lucerne.jpg"
           ]
       }
       `additionalKeys` myKeybindings xmobarSignal

  launch cfg dirs


startXmobar :: IO (STM.TQueue String, STM.TMVar Xmobar.SignalType, ThreadId)
startXmobar = do
  xmobarQueue <- STM.newTQueueIO
  xmobarSignal <- STM.newEmptyTMVarIO
  conf <- xmobarConf xmobarSignal xmobarQueue
  xmobarProc <- forkIO (Xmobar.xmobar conf)
  pure (xmobarQueue, xmobarSignal, xmobarProc)


-- ============== Layouts ==============


gaps ::
  LayoutClass l a =>
  Integer ->
  Integer ->
  l a ->
  ModifiedLayout Spacing (ModifiedLayout SmartBorder l) a
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


toggleBar :: STM.TMVar Xmobar.SignalType -> X ()
toggleBar xmobarSignal = do
  broadcastMessage ToggleStruts
  io (STM.atomically (STM.putTMVar xmobarSignal (Xmobar.Toggle 0)))
  refresh


myKeybindings :: STM.TMVar Xmobar.SignalType -> [((KeyMask, KeySym), X ())]
myKeybindings xmobarSignal =
  [ ( ( myModMask, xK_q ), fmap lines dmenuSelectXmonad >>= \case
        []       -> pure ()
        (xmnd:_) -> restart xmnd True
    )
  , ( ( myModMask .|. shiftMask, xK_q ), fmap lines dmenuSelectSession >>= \case
        []          -> pure ()
        (session:_) -> terminateSession session
    )
  , ( ( myModMask, xK_i ), spawn "dmenu_run -F -p open" )
  , ( ( myModMask,  xK_o )
    , dmenuGitDirs >>= \dir -> unless (null dir) $ tmuxNewSession dir
    )
  , ( ( myModMask, xK_u ) , spawn "/home/john/.local/bin/clipmenu -p clipboard" )
  -- , ( ( myModMask .|. shiftMask , xK_l ) , dmenuLPass )
  , ( ( myModMask .|. controlMask, xK_f) , toggleBar xmobarSignal )
  , ( ( myModMask .|. shiftMask, xK_x ), spawn "xlock -mode rain" )
  , ( ( myModMask .|. controlMask, xK_4 ), spawn "scrot" )
  , ( ( myModMask, xK_g )
    , gotoMenuConfig $ def
      { menuCommand = "dmenu"
      , menuArgs = [ "-f", "-F", "-p", "workspace" ]
      }
    )
  , ( ( myModMask, xK_b )
    , bringMenuConfig $ def
      { menuCommand = "dmenu"
      , menuArgs = [ "-f", "-F", "-p", "window" ]
      }
    )
  , ( ( 0, xF86XK_AudioLowerVolume )
    , spawn "pulsemixer --change-volume -2"
    )
  , ( ( 0, xF86XK_AudioRaiseVolume )
    , spawn "pulsemixer --change-volume +2"
    )
  , ( ( 0, xF86XK_AudioMute ), spawn "pulsemixer --toggle-mute" )
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
  , mconcat $ intersperse " "
    ["fd '\\.git' '/' -t d -H -I"
    , "-E '\\.github'"
    , "-E '\\.cache'"
    , "-E '\\.tmux'"
    , "-E '\\.cargo'"
    , "-E /gnu/store"
    , "-E '\\.git-credential-cache'"
    , "-E IndexedDB"
    , "-E '\\.spago'"
    , "-E '\\.emacs\\.d'"
    , "-E '\\.racket'"
    , "-E '/nix/store'"
    , "-E '/tmp'"
    , "-E '\\.local'"
    , "-E 'dist-newstyle'"
    , "-E '\\.opam'"
    , "-E '/s3'"
    , "| while read -r l; do echo \"${l%/.git/}\"; done"
    , "| dmenu -f -F -p 'repository'"
    ]
  ]
  ""


dmenuSelectSession :: X String
dmenuSelectSession =
  runProcessWithInput "bash"
  [ "-c"
  , mconcat $ intersperse " | "
    [ "loginctl list-sessions"
    , "rg tty"
    , "dmenu -f -F -p 'kill session'"
    , "gawk '{ print $1 }'"
    ]
  ]
  ""


terminateSession :: String -> X ()
terminateSession session = void $ runProcessWithInput "loginctl"
  [ "terminate-session"
  , session
  ]
  ""


dmenuSelectXmonad :: X String
dmenuSelectXmonad =
  runProcessWithInput "bash"
  [ "-c"
  , mconcat $ intersperse " | "
    [ "for f in ~/.{guix-profile,cabal}/bin/my-xmonad; do echo $f; done"
    , "dmenu -f -F -p 'restart with'"
    ]
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

xmobarScreenId :: Int
xmobarScreenId = 1


xmobarSegmentSep :: String
xmobarSegmentSep =  " | "


queueReaderWidget :: STM.TQueue String -> Template.Seg Template.RunnableWidget
queueReaderWidget xmobarQueue = Template.Runnable $ Template.RunnableWidget
  { Template.com = Xmobar.Run (Xmobar.QueueReader xmobarQueue id "xmonadstuff")
  , Template.runnableFormat = xmobarFmt
  }


dynNetworkWidget :: Template.Seg Template.RunnableWidget
dynNetworkWidget = Template.Runnable $ Template.RunnableWidget
  { Template.com = Xmobar.Run (Xmobar.DynNetwork [] 20)
  , Template.runnableFormat = xmobarFmt
  }


alsaWidget :: Template.Seg Template.RunnableWidget
alsaWidget = Template.Runnable $ Template.RunnableWidget
  { Template.com = Xmobar.Run
    (Xmobar.Alsa "default" "Master"
      [ "-t" , "<status> <volume>%"
      , "--"
      , "--alsactl=/home/john/.guix-profile/bin/pactl"
      , "--on", "vol", "--onc" , coerce base0
      , "--off", "vol", "--offc" , coerce red
      ])
  , Template.runnableFormat = xmobarFmt
    { Template.actions =
      Just [ Action.Spawn [1] "amixer -q set Master toggle" ]
    }
  }


mpdWidget :: Template.Seg Template.RunnableWidget
mpdWidget = Template.Runnable $ Template.RunnableWidget
  { Template.com = Xmobar.Run (Xmobar.MPD
     ["-t"
     , "<composer> <title> (<album>) <track>/<plength> <statei> [<flags>]"
     , "--", "-P", ">>", "-Z", "|", "-S", "><"
     ] 10)
  , Template.runnableFormat = xmobarFmt
  }


dateWidget :: Template.Seg Template.RunnableWidget
dateWidget = Template.Runnable $ Template.RunnableWidget
  { Template.com = Xmobar.Run (Xmobar.Date ("%F" <> xmobarSegmentSep <> "%r") "date" 10)
  , Template.runnableFormat = xmobarFmt
  }


xmobarFmt :: Template.Format
xmobarFmt = Template.Format
  { Template.fontIndex = 0
  , Template.textRenderInfo = Template.TextRenderInfo
    { Template.tColorsString = coerce base0 <> ",#002b36"
    , Template.tBgTopOffset = -1
    , Template.tBgBottomOffset = -1
    , Template.tBoxes = []
    }
  , actions = Nothing
  }


stdFormat :: Template.Widget -> Template.Seg a
stdFormat w = Template.Plain $ Template.PlainSeg
  { Template.widget = w
  , format = xmobarFmt
  }


separatorSeg :: Template.Seg a
separatorSeg = stdFormat (Template.Text " | ")


bar :: STM.TMVar Xmobar.SignalType -> STM.TQueue String -> Template.Bar Template.RunnableWidget
bar xmobarSignal xmobarQueue = Template.Bar
  { Template.left =
    [ stdFormat (Template.Text " λ ")
    , queueReaderWidget xmobarQueue
    ]
  , Template.center = (intersperse separatorSeg
    -- [ mpdWidget
    -- , alsaWidget
    [ dynNetworkWidget
    ])
  , Template.right = [ dateWidget ]
    <> [ stdFormat (Template.Text " ") ]
  }


xmobarConf :: STM.TMVar Xmobar.SignalType -> STM.TQueue String -> IO Xmobar.Config
xmobarConf xmobarSignal xmobarQueue =
  pure $ Xmobar.defaultConfig
    { Xmobar.font = "xft:Iosevka:size=12:light:antialias=true"
    , Xmobar.fgColor = coerce base0
    , Xmobar.bgColor = "#002b36"
    , Xmobar.borderColor = coerce base03
    , Xmobar.border = Xmobar.BottomB
    , Xmobar.alpha = 204
    , Xmobar.position = Xmobar.OnScreen xmobarScreenId (Xmobar.TopW Xmobar.L 100)
    , Xmobar.allDesktops = True
    , Xmobar.lowerOnStart = True
    , Xmobar.pickBroadest = False
    , Xmobar.persistent = False
    , Xmobar.hideOnStart = False
    , Xmobar.template = Xmobar.Parsed (bar xmobarSignal xmobarQueue)
    , Xmobar.signal = Xmobar.SignalChan (Just xmobarSignal)
    , Xmobar.verbose = True
    }


myXmobarPP :: STM.TQueue String -> X ()
myXmobarPP xmobarQueue = do
  Titles {..} <- withWindowSet allTitles

  let wsPrefix :: WorkspaceId -> String
      wsPrefix wsId =
        " " ++ show xmobarScreenId ++ "," ++ wsId ++ xmobarSegmentSep

  dynamicLogWithPP $ xmobarPP
    { ppOutput = \out ->
        STM.atomically
        $ STM.writeTQueue xmobarQueue
        $ show xmobarScreenId ++ ". " ++ out
    , ppCurrent = \wsId ->
        xmobarColor' base0 base01
        $ wsPrefix wsId ++ titleFormat current
    , ppHidden = \wsId ->
        xmobarAction ("xdotool key super+" ++ wsId) "1"
        $ xmobarColor' base01 base03
        $ wsPrefix wsId ++ hiddenTitle hidden wsId
    , ppVisible = \wsId ->
        xmobarColor' base01 base03
        $ wsPrefix wsId ++ titleFor visible wsId
    , ppUrgent = \wsId ->
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
    , ppOrder = \case
        (ws:_:t:e) -> e ++ [ ws, t ]
        x          -> x
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


red :: SpaceColor
red = SpaceColor "#dc322f"
