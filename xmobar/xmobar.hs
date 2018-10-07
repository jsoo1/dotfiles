Config
  { font =
    "xft:FantasqueSansMono Nerd Font Mono:size=12:normal:antialias=true"
  , additionalFonts = []
  , borderColor = "#002b36"
  , border = TopB
  , bgColor = "#002b36"
  , fgColor = "#839496"
  , alpha = 255
  , position = Top
  , textOffset = -1
  , iconOffset = -1
  , lowerOnStart = True
  , pickBroadest = False
  , persistent = False
  , hideOnStart = False
  , iconRoot = "."
  , allDesktops = True
  , overrideRedirect = True
  , commands =
    [ Run Weather
        "KSNA"
        [ "-t"
        , "<tempF>°F"
        , "-L"
        , "65"
        , "-H"
        , "85"
        , "--normal"
        , "green,#859900"
        , "--high"
        , "red,#cb4b16"
        , "--low"
        , "lightblue,#2aa198"
        ]
        9000
    , Run Wireless
      "wlp9s0"
      [ "-t"
      , "<quality>%  <essid>"
      ]
      200
    , Run Battery
        [ "-t"
        , "<left>%  <timeleft>  <acstatus>"
        , "--"
        , "-O"
        , "AC"
        , "-o"
        , "Bat"
        , "-h"
        , "green"
        , "-l"
        , "red"
        ]
        200
    -- Fix the alsa-mixer stuff
    -- , Run Volume
    --   "default"
    --   "Master"
    --   [ "-t"
    --   , " <volume>%  <status>  Vol "
    --   , "--"
    --   , "--on"
    --   , "on"
    --   , "--onc"
    --   , "#2D9574,#292b2e"
    --   , "-o"
    --   , "off"
    --   , "--offc"
    --   , "grey,#292b2e"
    --   ]
    --   200
    --     \<fc=#15171a,#292b2e></fc><fc=#292b2e,#15171a></fc><fc=#2D9574,#292b2e>%default:Master%</fc>\

    , Run Date "%l:%M %p  %D" "date" 600
    , Run StdinReader
    ]
  , sepChar = "%"
  , alignSep = "}{"
  , template =
    "%StdinReader%\
      \}{\
      \<fc=#839496,#002b36></fc><fc=#002b36,#839496> %wlp9s0wi% </fc><fc=#002b36,#839496></fc>\
      \<fc=#839496,#002b36></fc><fc=#002b36,#839496> %battery% </fc><fc=#002b36,#839496></fc>\
      \<fc=#839496,#002b36></fc><fc=#002b36,#839496> %date% </fc>"
  }
