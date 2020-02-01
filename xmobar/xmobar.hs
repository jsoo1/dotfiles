Config
  { font =
    "xft:Iosevka:size=14:normal:antialias=true"
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
      , "<essid> <quality>%"
      ]
      200
    , Run Com "hostname" [] "hostname" 0
    , Run Battery
        [ "-t"
        , "<left>%"
        , "--"
        , "-O"
        , "AC"
        , "-o"
        , "Bat"
        , "-h"
        , "#859900"
        , "-l"
        , "#dc322f"
        ]
        200
    , Run Volume
      "default"
      "Master"
      [ "-t"
      , "<volume>% | <status> | vol"
      , "--"
      , "--on"
      , "on"
      , "--onc"
      , "#002b36,#586e75"
      , "-o"
      , "off"
      , "--offc"
      , "#002b36,#dc322f"
      ]
      200
    , Run Date "%Y-%m-%d | %l:%M %p " "date" 600
    , Run StdinReader
    ]
  , sepChar = "%"
  , alignSep = "}{"
  , template =
    "<fc=#002b36,#859900> λ</fc>%StdinReader%\
      \}{\
      \<action=`amixer -q set Master toggle`><fc=#002b36,#586e75> %default:Master% </fc></action>\
      \<fc=#586e75,#002b36> %wlp9s0wi% </fc>\
      \<fc=#586e75,#002b36>| bat %battery% </fc>\
      \<fc=#002b36,#586e75> %date% </fc>"
  }
