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
    -- screenshot
  , ("M-<Print>", spawn "scrot screenshot-%Y-%m-%d-%s.png")
    -- screencam
  , ("M-C-<Print>",
     spawn $
     "vlc screen://" ++
     " --screen-fps=12" ++
     " --input-slave=alsa://hw:1,0" ++
     " --qt-start-minimized" ++
     " --sout-transcode-high-priority" ++
     " --sout \"#transcode" ++
     "{venc=x264" ++
     ",vcodec=h264" ++
     ",fps=12" ++
     ",vb=640" ++
     ",acodec=acc" ++
     ",channels=1" ++
     ",ab=64}" ++
     ":std" ++
     "{access=file" ++
     ",mux=mp4" ++
     ",dst=screencam-$(date -u +%Y-%m-%d-%s).mp4}\"")
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
