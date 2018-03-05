Config
  { font =
    "xft:FantasqueSansMono Nerd Font Mono:size=15:normal:antialias=true"
  , additionalFonts = []
  , borderColor = "#15171a"
  , border = TopB
  , bgColor = "#15171a" -- "#292b2e"
  , fgColor = "grey"
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
        , "green,#5d4d7a"
        , "--high"
        , "red,#ce537a"
        , "--low"
        , "lightblue,#5d4d7a"
        ]
        9000
    , Run Wireless
      "wlo1"
      [ "-t"
      , "<quality>  <essid>"
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
    , Run Volume
      "default"
      "Master"
      [ "-t"
      , " <volume>  <status> Vol"
      -- , "--on"
      -- , "on"
      -- , "--onc"
      -- , "green,#15171a"
      -- , "-o"
      -- , "off"
      -- , "--offc"
      -- , "grey,#15171a"
      ]
      200
    , Run Date "%l:%M %p  %D" "date" 10
    , Run StdinReader
    ]
  , sepChar = "%"
  , alignSep = "}{"
  , template =
    "<fc=#15171a,grey> %whoami%@%hostname% </fc>\
    \<fc=grey,#15171a></fc>%StdinReader%\
    \}{\
    \<fc=#4f97d7>%default:Master% </fc>\
    \<fc=#2D9574> %wlo1wi% </fc>\
    \<fc=#2D9574,#15171a></fc><fc=#15171a,#2D9574> %battery% </fc>\
    \<fc=#15171a,#2D9574></fc><fc=#4f97d7,#15171a></fc><fc=#15171a,#4f97d7>%date% </fc>"
  }
