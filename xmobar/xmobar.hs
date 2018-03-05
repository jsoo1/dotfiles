Config
  { font =
    "xft:FantasqueSansMono Nerd Font Mono:size=16:normal:antialias=true"
  , additionalFonts = []
  , borderColor = "#292b2e"
  , border = TopB
  , bgColor = "#292b2e"
  , fgColor = "grey"
  , alpha = 255
  , position = Bottom
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
        , "green"
        , "--high"
        , "red"
        , "--low"
        , "lightblue"
        ]
        36000
    , Run Network
        "lo"
        [ "-L", "0", "-H", "32", "--normal", "green", "--high", "red" ]
        10
    , Run Network
        "wlo1"
        [ "-L", "0", "-H", "32", "--normal", "green", "--high", "red" ]
        10
    , Run Cpu
        [ "-L", "3", "-H", "50", "--normal", "green", "--high", "red" ]
        10
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
        ] 10
    , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
    , Run StdinReader
    ]
  , sepChar = "%"
  , alignSep = "}{"
  , template =
    "<fc=#292b2e,grey> Workspaces </fc><fc=grey,#15171a></fc><fc=#15171a,#292b2e></fc> %StdinReader% }{<fc=#4f97d7,grey></fc> <fc=grey,#4f97d7>%lo% - %wlo1%</fc>  <fc=#2D9574>%battery%</fc>  <fc=#ce537a>%KSNA%</fc>  <fc=#4f97d7>%date%</fc> "
  }
