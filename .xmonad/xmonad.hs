{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

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
import           XMonad.Layout.BorderResize
import           XMonad.Layout.BoringWindows
import           XMonad.Layout.ButtonDecoration
import           XMonad.Layout.CenteredMaster
import           XMonad.Layout.Circle
import           XMonad.Layout.Column
import           XMonad.Layout.Combo
import           XMonad.Layout.ComboP
import           XMonad.Layout.Cross
import           XMonad.Layout.Decoration
import           XMonad.Layout.DecorationAddons
import           XMonad.Layout.DecorationMadness
import           XMonad.Layout.Dishes
import           XMonad.Layout.DragPane
import           XMonad.Layout.DraggingVisualizer
import           XMonad.Layout.Drawer
import           XMonad.Layout.DwmStyle
import           XMonad.Layout.FixedColumn
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Gaps hiding (L)
import           XMonad.Layout.Grid
import           XMonad.Layout.GridVariants hiding (Grid)
import           XMonad.Layout.Groups
import           XMonad.Layout.Groups
import           XMonad.Layout.Groups.Examples
import           XMonad.Layout.Groups.Helpers
import           XMonad.Layout.Groups.Wmii
import           XMonad.Layout.HintedGrid hiding (Grid, GridRatio)
import           XMonad.Layout.HintedTile hiding (Tall)
import           XMonad.Layout.IM
import           XMonad.Layout.ImageButtonDecoration
import           XMonad.Layout.IndependentScreens
import           XMonad.Layout.LayoutBuilder
import           XMonad.Layout.LayoutBuilderP
import           XMonad.Layout.LayoutCombinators hiding ((|||))
import           XMonad.Layout.LayoutHints
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.LayoutScreens
import           XMonad.Layout.LimitWindows
import           XMonad.Layout.MagicFocus
import           XMonad.Layout.Magnifier
import           XMonad.Layout.Master
import           XMonad.Layout.Maximize
import           XMonad.Layout.MessageControl
import           XMonad.Layout.Minimize
import           XMonad.Layout.Monitor
import           XMonad.Layout.Mosaic
import           XMonad.Layout.MosaicAlt
import           XMonad.Layout.MouseResizableTile
import           XMonad.Layout.MultiColumns
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.Named
import           XMonad.Layout.NoBorders
import           XMonad.Layout.NoFrillsDecoration
import           XMonad.Layout.OnHost
import           XMonad.Layout.OneBig
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.PositionStoreFloat
import           XMonad.Layout.Reflect
import           XMonad.Layout.Renamed
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.ResizeScreen
import           XMonad.Layout.Roledex
import           XMonad.Layout.ShowWName
import           XMonad.Layout.SimpleDecoration
import           XMonad.Layout.SimpleFloat
import           XMonad.Layout.Simplest
import           XMonad.Layout.SimplestFloat
import           XMonad.Layout.Spacing
import           XMonad.Layout.Spiral
import           XMonad.Layout.Square
import           XMonad.Layout.StackTile
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.TabBarDecoration
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.ToggleLayouts
import           XMonad.Layout.TwoPane
import           XMonad.Layout.WindowArranger
import           XMonad.Layout.WindowNavigation hiding (L)
import           XMonad.Layout.WindowSwitcherDecoration
import           XMonad.Layout.WorkspaceDir
import           XMonad.Layout.ZoomRow
import           XMonad.Prompt
import           XMonad.Prompt.AppendFile
import           XMonad.Prompt.Shell
import           XMonad.Util.Replace

main :: IO ()
main =
  replace >>
  xmonad conf { modMask    = modm
           -- , terminal   = "urxvt"
              , layoutHook = myLayoutHook conf
              , manageHook = myManageHook <+> manageHook conf
              , keys       = myKeys <+> keys conf
              }
  where
    conf = gnomeConfig
    modm = mod4Mask
    myLayoutHook conf' =
   -- onWorkspace "9" (withIM (1%7) (Title "tim.dysinger - Skype™") (Tall 1 0.5 0.5)) $
   -- showWName $
   -- noFrillsDeco shrinkText defaultTheme $
      minimize $
      maximize $
   -- magnifier $
   -- spacing 2 $
      mouseResizableTile
      ||| layoutHook conf'
      ||| Accordion
      ||| Circle
      ||| Column 1.6
      ||| Dishes 2 (1/6)
      ||| FixedColumn 1 20 80 10
      ||| Grid
      ||| GridRatio (4/3)
      ||| MosaicAlt DM.empty
      ||| ResizableTall 1 (3/100) (1/2) []
      ||| Roledex
      ||| Simplest
      ||| SplitGrid L 2 3 (2/3) (16/10) (5/100)
      ||| ThreeCol 1 (3/100) (1/2)
      ||| TwoPane (3/100) (1/2)
      ||| autoMaster 1 (1/100) Grid
      ||| centerMaster Grid
      ||| dragPane Horizontal 0.1 0.5
      ||| mosaic 2 [3,2]
      ||| multiCol [1] 4 0.01 0.5
      ||| reflectHoriz (Tall 1 (3/100) (1/2))
      ||| simpleCross
      ||| simpleDrawer 0.01 0.3 (ClassName "Rhythmbox") `onTop` (Tall 1 0.03 0.5)
      ||| simpleFloat
      ||| spiral (6/7)
    myManageHook =
      composeAll [ className =? "Dia"      --> doFloat
                 , className =? "Gimp-2.6" --> doFloat
                 , className =? "Glade"    --> doFloat
                 , className =? "Synapse"  --> doIgnore
                 , isFullscreen            --> doFullFloat
                 ]
    myKeys _conf@(XConfig { XMonad.modMask = modm'}) =
      DM.fromList $
      [ ((modm', xK_z), toggleWS)
      , ((modm', xK_backslash), withFocused (sendMessage . maximizeRestore))
   -- , ((modm',               xK_m     ), withFocused minimizeWindow)
   -- , ((modm' .|. shiftMask, xK_m     ), sendMessage RestoreNextMinimizedWin)
      , ((modm', xK_s), sinkAll)
      , ((modm', xK_f), spawn "firefox")
      , ((modm', xK_Print), spawn "scrot screenshot-%Y-%m-%d-%s.png")
      , ((modm' .|. controlMask, xK_Print),
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
      , ((modm', xK_Home), spawn "nautilus $HOME || thunar $HOME")
      , ((modm', xK_n), appendFilePrompt myXPConfig "org/inbox.org")
      , ((modm' .|. controlMask, xK_space), shellPrompt myXPConfig)
      ]
      -- viewing and shifting to the prev workspace
      ++ [ ((modm' .|. mask, key), action)
         | key <- [xK_Left, xK_Up]
         , (action, mask) <- [(prevWS, 0), (shiftToPrev, shiftMask)]
         ]
      -- viewing and shifting to the next workspace
      ++ [ ((modm' .|. mask, key), action)
         | key <- [xK_Right, xK_Down]
         , (action, mask) <- [(nextWS, 0), (shiftToNext, shiftMask)]
         ]
      -- viewing & sending windows to xinerama screens
      ++ [ ((modm' .|. mask, key), action screen)
         | (key, screen) <- zip [xK_apostrophe, xK_comma, xK_period] [0..]
         , (action, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]
         ]
    myXPConfig =
      config' { autoComplete = Just 1
           -- , font         = myFont
              , position     = Bottom }
      where
        config' = defaultXPConfig
     -- myFont = "xft:Inconsolata:bold:size=11:antialias=true:hinting=light"
