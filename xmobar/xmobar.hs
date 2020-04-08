Config
  { font =
    "xft:Iosevka Term Slab:size=15:light:antialias=true"
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
    [ Run Wireless
      "wlp9s0"
      [ "-t"
      , "wifi <quality>%"
      ]
      200
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
    , Run Alsa
      "default"
      "Master"
      [ "-t"
      , "<status> <volume>%"
      , "--"
      , "--on"
      , "vol"
      , "--onc"
      , "#586e75,#002b36"
      , "-o"
      , "vol"
      , "--offc"
      , "#dc322f,#002b36"
      ]
    , Run Date "%Y-%m-%d | %l:%M %p " "date" 600
    , Run UnsafeStdinReader
    , Run Com "light" [] "light" 2
    ]
  , sepChar = "%"
  , alignSep = "}{"
  , template =
    "<fc=#002b36,#859900> λ</fc>%UnsafeStdinReader%\
      \}{\
      \<fc=#002b36,#586e75> light %light% </fc>\
      \<action=`amixer -q set Master toggle`><fc=#586e75,#002b36> %alsa:default:Master% </fc></action>\
      \<fc=#002b36,#586e75> %wlp9s0wi% </fc>\
      \<fc=#002b36,#586e75>| bat %battery% </fc>\
      \<fc=#586e75,#002b36> %date% </fc>"
  }
