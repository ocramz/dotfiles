import qualified Data.Map                               as DM
import           Data.Ratio                             ((%))
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.PhysicalScreens
import           XMonad.Actions.WithAll
import           XMonad.Config.Gnome
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.Accordion
import           XMonad.Layout.AutoMaster
import           XMonad.Layout.ButtonDecoration
import           XMonad.Layout.Circle
import           XMonad.Layout.Column
import           XMonad.Layout.Cross
import           XMonad.Layout.DecorationAddons
import           XMonad.Layout.Dishes
import           XMonad.Layout.DragPane
import           XMonad.Layout.DraggingVisualizer
import           XMonad.Layout.FixedColumn
import           XMonad.Layout.Gaps
import qualified XMonad.Layout.Grid                     as XLG
import qualified XMonad.Layout.GridVariants             as XLGV
import qualified XMonad.Layout.HintedGrid               as XLHG
import qualified XMonad.Layout.HintedTile               as XLHT
import           XMonad.Layout.IM
import           XMonad.Layout.Maximize
import           XMonad.Layout.Minimize
import           XMonad.Layout.MouseResizableTile
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Roledex
import           XMonad.Layout.Spiral
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.WindowSwitcherDecoration
import           XMonad.Prompt
import           XMonad.Prompt.AppendFile
import           XMonad.Prompt.Shell
import           XMonad.Util.Replace

-- TODO read more
--   http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Doc-Extending.html

main :: IO ()
main =
  replace >>
  xmonad conf { modMask    = modm
           -- , terminal   = "urxvt"
              , layoutHook = myLayoutHook conf
              , manageHook = myManageHook <+> manageHook conf
              , keys       = myKeys <+> keys conf
              }
  where conf = gnomeConfig
        modm = mod4Mask

myLayoutHook conf =
  -- buttonDeco shrinkText defaultThemeWithButtons $
  -- windowSwitcherDecoration shrinkText defaultTheme $
  -- draggingVisualizer $
  -- onWorkspace "1" (withIM (1%7) (Title "Downloads") (Tall 1 0.5 0.5)) $
  onWorkspace "9" (withIM (1%7) (Title "tim.dysinger - Skype™") (Tall 1 0.5 0.5)) $
  minimize $
  maximize $
  layoutHook conf
  ||| spiral (6/7)
  ||| XLG.Grid
  -- ||| mouseResizableTile
  -- ||| Accordion
  -- ||| Circle
  -- ||| Column 1.6
  -- ||| Dishes 2 (1/6)
  -- ||| FixedColumn 1 20 80 10
  -- ||| Roledex
  -- ||| ThreeCol 1 (3/100) (1/2)
  -- ||| XLGV.Grid (16/10)
  -- ||| XLHG.Grid False
  -- ||| XLHT.HintedTile 1 (1/2) (3/100) XLHT.Tall
  -- ||| XLHT.HintedTile 1 (1/2) (3/100) XLHT.Wide
  -- ||| dragPane Horizontal 0.1 0.5
  -- ||| dragPane Vertical 0.1 0.5
  -- ||| simpleCross
  -- ||| simpleTabbed

myManageHook =
  composeAll [ className =? "Dia"      --> doFloat
             , className =? "Gimp-2.6" --> doFloat
             , className =? "Glade"    --> doFloat
             , className =? "Synapse"  --> doIgnore
             , isFullscreen            --> doFullFloat
             ]

myKeys conf@(XConfig { XMonad.modMask = modm}) =
  DM.fromList $
  [ ((modm, xK_z), toggleWS)
  , ((modm, xK_backslash), withFocused (sendMessage . maximizeRestore))
  -- , ((modm,               xK_m     ), withFocused minimizeWindow)
  -- , ((modm .|. shiftMask, xK_m     ), sendMessage RestoreNextMinimizedWin)
  , ((modm, xK_s), sinkAll)
  , ((modm, xK_f), spawn "firefox")
  , ((modm, xK_Print), spawn "scrot screenshot-%Y-%m-%d-%s.png")
  , ((modm .|. controlMask, xK_Print),
     spawn $
     "vlc screen://" ++
     " --screen-fps=12" ++
     " --input-slave=alsa://" ++
     " --qt-start-minimized" ++
     " --sout-transcode-high-priority" ++
     " --sout \"" ++
     "#transcode{venc=x264,vcodec=h264,fps=12,vb=640,acodec=mp3,channels=1,ab=64}" ++
     ":standard{access=file,mux=mp4,dst=screencast-$(date -u +%Y-%m-%d-%s).avi}" ++
     "\""
    )
  , ((modm, xK_Home), spawn "nautilus $HOME || thunar $HOME")
  , ((modm, xK_n), appendFilePrompt myXPConfig "org/inbox.org")
  , ((modm .|. controlMask, xK_space), shellPrompt myXPConfig)
  ]
  -- viewing and shifting to the prev workspace
  ++ [ ((modm .|. mask, key), action)
     | key <- [xK_Left, xK_Up]
     , (action, mask) <- [(prevWS, 0), (shiftToPrev, shiftMask)]
     ]
  -- viewing and shifting to the next workspace
  ++ [ ((modm .|. mask, key), action)
     | key <- [xK_Right, xK_Down]
     , (action, mask) <- [(nextWS, 0), (shiftToNext, shiftMask)]
     ]
  -- viewing & sending windows to xinerama screens
  ++ [ ((modm .|. mask, key), action screen)
     | (key, screen) <- zip [xK_apostrophe, xK_comma, xK_period] [0..]
     , (action, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]
     ]

myXPConfig =
  config { autoComplete = Just 1
      -- , font         = myFont
         , position     = Bottom
         }
  where config = defaultXPConfig

-- myFont = "xft:Inconsolata:bold:size=11:antialias=true:hinting=light"
