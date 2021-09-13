Config
  { font = "xft:Iosevka:size=12:light:antialias=true"
  , additionalFonts = []
  , borderColor = "#002b36"
  , border = BottomB
  , bgColor = "#00362b"
  , fgColor = "#839496"
  , alpha = 204
  , position = Top
  , textOffset = -1
  , iconOffset = -1
  , lowerOnStart = True
  , pickBroadest = False
  , persistent = False
  , hideOnStart = False
  , iconRoot = "."
  , commands =
    [ Run Wireless "wlp9s0" [ "-t" , "wifi <quality>%"] 200
    , Run Battery
      [ "-t" , "<acstatus> <left>%"
      , "--"
      , "-O" , "ac"
      , "-i", "full"
      , "-o" , "bat"
      , "-h" , "#859900"
      , "-l" , "#dc322f"
      ]
      20
    , Run Alsa "default" "Master"
      [ "-t" , "<status> <volume>%"
      , "--"
      , "--on", "vol", "--onc" , "#839496"
      , "-o", "vol", "--offc" , "#dc322f"
      ]
    , Run Date "%F | %r" "date" 600
    , Run UnsafeStdinReader
    ]
  , alignSep = "}{"
  , template =
    " λ %UnsafeStdinReader% \
    \}{<action=`amixer -q set Master toggle`>%alsa:default:Master%</action>\
    \ | %wlp9s0wi% | %battery% | %date%  "
  }
