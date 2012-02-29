import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.WithAll
import XMonad.Config.Xfce
import XMonad.Hooks.ManageHelpers
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.CustomKeys

main = xmonad myConfig
  where
    myConfig     = xfceConfig { modMask    = myMask
                              , keys       = myKeys
                              , manageHook = myManageHook
                              }
    myXPConfig   = greenXPConfig { autoComplete = Just 1
                                 , font         = myFont
                                 , position     = Bottom
                                 }
    myManageHook = manageHook xfceConfig <+> composeAll
                   [ className =? "Xfce4-notifyd" --> doIgnore
                   , doCenterFloat
                   ]
    myKeys       = customKeys (\_ -> []) (\_ -> myKeysToAdd)
    myRunMask    = myMask .|. controlMask
    myMask       = mod4Mask
    myFont       = "xft:inconsolata:bold:size=13:antialias=true:hinting=light"
    myKeysToAdd  =
      [ ((myMask,               xK_Right), nextWS)
      , ((myMask,               xK_Down),  nextWS)
      , ((myMask,               xK_Left),  prevWS)
      , ((myMask,               xK_Up),    prevWS)
      , ((myMask .|. shiftMask, xK_Right), shiftToNext)
      , ((myMask .|. shiftMask, xK_Down),  shiftToNext)
      , ((myMask .|. shiftMask, xK_Left),  shiftToPrev)
      , ((myMask .|. shiftMask, xK_Up),    shiftToPrev)
      , ((myMask, xK_a), sinkAll)
      , ((myMask, xK_z), toggleWS)
      , ((myMask, xK_Print), spawn "scrot ~/screenshot-%Y-%m-%d.png")
      , ((myRunMask, xK_space), shellPrompt myXPConfig)
      , ((myRunMask, xK_f), spawn "firefox")
      , ((myRunMask, xK_h), spawn "thunar ~")
      ]
