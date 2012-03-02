import Yi
import Data.Monoid

-- SOLARIZED HEX     16/8 TERMCOL  XTERM/HEX   L*A*B      RGB         HSB
-- --------- ------- ---- -------  ----------- ---------- ----------- -----------
-- base00    #657b83 11/7 bryellow 241 #626262 50 -07 -07 101 123 131 195  23  51
-- base01    #586e75 10/7 brgreen  240 #585858 45 -07 -07  88 110 117 194  25  46
-- base02    #073642  0/4 black    235 #262626 20 -12 -12   7  54  66 192  90  26
-- base03    #002b36  8/4 brblack  234 #1c1c1c 15 -12 -12   0  43  54 193 100  21
-- base0     #839496 12/6 brblue   244 #808080 60 -06 -03 131 148 150 186  13  59
-- base1     #93a1a1 14/4 brcyan   245 #8a8a8a 65 -05 -02 147 161 161 180   9  63
-- base2     #eee8d5  7/7 white    254 #e4e4e4 92 -00  10 238 232 213  44  11  93
-- base3     #fdf6e3 15/7 brwhite  230 #ffffd7 97  00  10 253 246 227  44  10  99
-- blue      #268bd2  4/4 blue      33 #0087ff 55 -10 -45  38 139 210 205  82  82
-- cyan      #2aa198  6/6 cyan      37 #00afaf 60 -35 -05  42 161 152 175  74  63
-- green     #859900  2/2 green     64 #5f8700 60 -20  65 133 153   0  68 100  60
-- magenta   #d33682  5/5 magenta  125 #af005f 50  65 -05 211  54 130 331  74  83
-- orange    #cb4b16  9/3 brred    166 #d75f00 50  50  55 203  75  22  18  89  80
-- red       #dc322f  1/1 red      160 #d70000 50  65  45 220  50  47   1  79  86
-- violet    #6c71c4 13/5 brmagenta 61 #5f5faf 50  15 -45 108 113 196 237  45  77
-- yellow    #b58900  3/3 yellow   136 #af8700 60  10  65 181 137   0  45 100  71

sBase00  = RGB 101 123 131 -- bryellow
sBase01  = RGB  88 110 117 -- brgreen
sBase02  = RGB   7  54  66 -- black
sBase03  = RGB   0  43  54 -- brblack
sBase0   = RGB 131 148 150 -- brblue
sBase1   = RGB 147 161 161 -- brcyan
sBase2   = RGB 238 232 213 -- white
sBase3   = RGB 253 246 227 -- brwhite
sBlue    = RGB  38 139 210 -- blue
sCyan    = RGB  42 161 152 -- cyan
sGreen   = RGB 133 153   0 -- green
sMagenta = RGB 211  54 130 -- magenta
sOrange  = RGB 203  75  22 -- brred
sRed     = RGB 220  50  47 -- red
sViolet  = RGB 108 113 196 -- brmagenta
sYellow  = RGB 181 137   0 -- yellow

solarizedTheme = defaultTheme `override` \super _ -> super
  { modelineAttributes = emptyAttributes
    { foreground = sBlue, background = sCyan }
  , modelineFocusStyle = withFg sBase3

  , tabBarAttributes   = emptyAttributes
    { foreground = sBlue, background = sBase3 }
  , tabInFocusStyle    = withFg grey `mappend` withBg sBase3
  , tabNotFocusedStyle = withFg lightGrey `mappend` withBg sBase2

  , baseAttributes     = emptyAttributes
    { foreground = sBase2, background = sBase02 }

  , selectedStyle      = withFg black `mappend` withBg sCyan
  , eofStyle           = withFg sBase0
  , errorStyle         = withBg sOrange
  , hintStyle          = withFg black `mappend` withBg sCyan
  , strongHintStyle    = withFg black `mappend` withBg sViolet

  , commentStyle       = withFg sMagenta
  , blockCommentStyle  = withFg sMagenta
  , keywordStyle       = withFg sBlue
  , numberStyle        = withFg sRed
  , preprocessorStyle  = withFg sOrange
  , stringStyle        = withFg sCyan
  , longStringStyle    = mempty
  , typeStyle          = withFg sGreen
  , dataConstructorStyle
                       = withBd True `mappend` withFg sGreen
  , importStyle        = withFg grey
  , builtinStyle       = withFg sBase0
  , regexStyle         = withFg sOrange
  , variableStyle      = mempty
  , operatorStyle      = withFg sYellow
  , makeFileRuleHead   = withFg sBase0
  , makeFileAction     = withFg grey
  , quoteStyle         = withFg grey
  }

main :: IO ()
main = yi $ defaultConfig -- { configUI = ui }
  where defaultConfig = defaultEmacsConfig
        ui = (configUI defaultConfig) { configTheme = solarizedTheme }
