import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.WithAll
import XMonad.Config.Xfce
import XMonad.Hooks.ManageHelpers
import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig

myConfig =
  xfceConfig { modMask    = mod4Mask
             , manageHook = manageHook xfceConfig <+> myManageHook
             } `additionalKeysP` myKeys

myManageHook =
  composeAll [ className =? "Xfce4-notifyd" --> doIgnore
             , doCenterFloat
             ]

myKeys =
  [ -- workspaces
    ("M-<Up>", prevWS)
  , ("M-<Left>", prevWS)
  , ("M-<Right>", nextWS)
  , ("M-<Down>", nextWS)
  , ("M-z", toggleWS)
    -- window management
  , ("M-S-<Up>", shiftToPrev)
  , ("M-S-<Left>", shiftToPrev)
  , ("M-S-<Right>", shiftToNext)
  , ("M-S-<Down>", shiftToNext)
  , ("M-s", sinkAll)
    -- quick-launch
  , ("M-f", spawn "firefox")
  , ("M-<Print>", spawn "scrot screenshot-%Y-%m-%d.png")
  , ("M-<Home>", spawn "thunar")
    -- quick-prompts
  , ("M-n", appendFilePrompt myXPConfig "org/inbox.org")
  , ("M-C-<Space>", shellPrompt myXPConfig)
  ]

myXPConfig =
  greenXPConfig { autoComplete = Just 1
                , font         = myFont
                , position     = Bottom
                }
myFont = "xft:inconsolata:bold:size=13:antialias=true:hinting=light"

main = xmonad myConfig
