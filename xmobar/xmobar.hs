Config
  { font =
    "xft:FantasqueSansMono Nerd Font Mono:size=16:normal:antialias=true"
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
        , "Santa Ana: <tempF>°F"
        , "-L"
        , "65"
        , "-H"
        , "85"
        , "--normal"
        , "green,#ce537a"
        , "--high"
        , "red,#ce537a"
        , "--low"
        , "lightblue,#ce537a"
        ]
        36000
    , Run Network
        "lo"
        [ "-L", "0", "-H", "32", "--normal", "green,#4f97d7", "--high", "red,#4f97d7" ]
        200
    , Run Network
        "wlo1"
        [ "-L", "0", "-H", "32", "--normal", "green,#4f97d7", "--high", "red,#4f97d7" ]
        200
    , Run Battery
        [ "-t"
        , "<acstatus>: <left>% - <timeleft>"
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
    , Run Date "%a %b%_d %H:%M:%S" "date" 10
    , Run StdinReader
    ]
  , sepChar = "%"
  , alignSep = "}{"
  , template =
    "<fc=#15171a,grey> %whoami%@%hostname% </fc><fc=grey,#15171a></fc>%StdinReader%}{<fc=#4f97d7,#15171a></fc><fc=#15171a,#4f97d7> %lo% - %wlo1% </fc><fc=#2D9574,#4f97d7></fc><fc=#15171a,#2D9574> %battery% </fc><fc=#ce537a,#2D9574></fc><fc=#15171a,#ce537a> %KSNA% </fc><fc=#4f97d7,#ce537a></fc><fc=#15171a,#4f97d7> %date% </fc>"
  }
