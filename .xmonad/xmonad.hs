import XMonad
import XMonad.Actions.CycleWS
import XMonad.Config.Gnome
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import XMonad.Util.CustomKeys

myFont = "xft:inconsolata:bold:size=13:antialias=true:hinting=true"

myXPConfig =
  greenXPConfig { autoComplete = Just 1
                , font = myFont
                , position = Bottom }

myMask = mod4Mask
myCmdMask = myMask .|. controlMask
myKeys =
  [ ((myMask, xK_Right), nextWS)
  , ((myMask, xK_Left), prevWS)
  , ((myMask .|. shiftMask, xK_Right), shiftToNext)
  , ((myMask .|. shiftMask, xK_Left), shiftToPrev)
  , ((myMask, xK_z), toggleWS)
  , ((myMask, xK_Print), spawn "scrot ~/screenshot-%Y-%m-%d.png")
  , ((myCmdMask, xK_space), shellPrompt myXPConfig)
  , ((myCmdMask, xK_m), manPrompt myXPConfig)
  , ((myCmdMask, xK_s), sshPrompt myXPConfig)
  , ((myCmdMask, xK_x), xmonadPrompt myXPConfig)
 ]

myTerminal = "urxvt -bg black -fg grey +sb"

myConfig =
  defaultConfig { modMask  = myMask
                , keys     = customKeys (\_ -> []) (\_ -> myKeys)
                , terminal = myTerm
                }

myGnomeConfig =
  gnomeConfig { modMask  = myMask
              , keys     = customKeys (\_ -> []) (\_ -> myKeys)
              }

main = xmonad myGnomeConfig
