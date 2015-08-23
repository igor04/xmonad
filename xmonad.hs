{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

-- import XMonad.Layout.StackTile     -- master window and fixed stack << spiral (4/5)
-- import XMonad.Layout.Spiral        -- show window to spirale direction << StackTile 1 (3/100) (1/2)
-- import XMonad.Layout.SimpleFloat   -- float window
-- import XMonad.Layout.Accordion
-- import XMonad.Layout.Circle
-- import XMonad.Layout.ThreeColumns  -- Mirror (multiCol [1] 2 0.01 (-0.25))
-- import XMonad.Layout.MultiColumns  -- ThreeCol 1 (3/100) (1/2)

-- Imports {{{
import XMonad
import XMonad.Operations
import Control.Monad (mapM_)

import System.IO
import System.Exit

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Dmenu

import XMonad.Actions.CycleWS         -- move focus, workspace, screens
import XMonad.Actions.CopyWindow      -- copy window to few workspace
import XMonad.Actions.GridSelect

import XMonad.Hooks.ManageDocks       -- avoidStruct - manage dock type programs
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog        -- log for dzen2
import XMonad.Hooks.UrgencyHook       -- work with urgency window
import XMonad.Hooks.FadeInactive      -- fadeInactiveLogHook work with xcompmgr

import XMonad.Layout.NoBorders (smartBorders, noBorders) -- smartborders give some issue with multiple screen
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)  -- can set loyaut for wrokspace
import XMonad.Layout.Spacing          -- can set free space between window
import XMonad.Layout.ResizableTile    -- MirrorShrink, MirrorExpand
import XMonad.Layout.Grid
import XMonad.Layout.Named            -- set name layout
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation -- needed for subLayout
import XMonad.Layout.Simplest         -- simple window
import XMonad.Layout.Magnifier
import XMonad.Layout.Tabbed
import XMonad.Layout.Gaps

import Data.Ratio ((%))

import qualified XMonad.StackSet as W
import qualified Data.Map as M
--}}}


-- Config {{{
myTerminal = "urxvtc"
myBitmapsDir = "/home/xim/.xmonad/panels/icon/"

dmenuBar        = "dmenu_run -nb '#000000' -nf '#7b7b7b' -sb '#005800' -sf '#aaaaaa' -fn 'Droid Sans Mono for Powerline':size=8"
logBarScreen1   = "dzen2 -xs 2 -x '0' -y '0' -h '13' -w '1320' -ta 'l' -fg '#ffffff' -bg '#000000' -fn 'Droid Sans Mono':bold:size=8"
logBarScreen2   = "dzen2 -xs 1 -x '0' -y '0' -h '13' -w '1366' -ta 'l' -fg '#ffffff' -bg '#000000' -fn 'Droid Sans Mono':bold:size=8"
infoBarScreen1  = "conky -c ~/.xmonad/panels/conkyrc | dzen2 -xs 2 -y 900 -h 13 -w 1440 -ta 'l' -fg '#444444' -bg '#000000' -fn 'Droid Sans Mono for Powerline':size=8"
infoBarScreen2  = "conky -c ~/.xmonad/panels/conkyrc | dzen2 -xs 1 -y 768 -h 13 -w 1366 -ta 'l' -fg '#444444' -bg '#000000' -fn 'Droid Sans Mono for Powerline':size=8"
-- infoBarScreen1  = "xmobar -x 1 ~/.xmonad/panels/xmobarrc"
-- infoBarScreen2  = "xmobar -x 2 ~/.xmonad/panels/xmobarrc"

ws1 = "❯ـ"
ws2 = "νim"
ws3 = "ìnet"
ws4 = "skype"
ws5 = "^i(" ++ myBitmapsDir ++ "phones.xbm)"
ws6 = "➏"
ws7 = "➐"
ws8 = "➑"
ws9 = "☻"
-- }}}


-- Main {{{
main = do
    logBarScreen1 <- spawnPipe logBarScreen1
    logBarScreen2 <- spawnPipe logBarScreen2

    xmonad defaultConfig
      { terminal            = myTerminal
      , workspaces          = [ws1, ws2, ws3, ws4, ws5, ws6, ws7, ws8, ws9]
      , keys                = keys'
      , modMask             = mod4Mask
      , layoutHook          = layoutHook'
      , manageHook          = manageHook'
      , startupHook         = startupHook'
      , logHook             = logHook' [logBarScreen1, logBarScreen2]  >> fadeInactiveLogHook 0xdddddddd
      , normalBorderColor   = colorNormalBorder
      , focusedBorderColor  = colorFocusedBorder
      , borderWidth         = 1
     }
-- }}}


-- Startup Hook {{{
startupHook' :: X()
startupHook' = do
  spawn infoBarScreen1
  spawn infoBarScreen2

restartHook :: X()
restartHook = do
  spawn "pkill xmobar"
  spawn "pkill conky"
-- }}}


-- Log Hook {{{
logHook' :: [Handle] -> X ()
logHook' handlers = do

  copies <- wsContainingCopies
  let check ws | ws `elem` copies = dzenColor color_ppCopy color_bgInactive . pad $ ws
               | otherwise = pad ws

  let icon = "^i(" ++ myBitmapsDir ++ "corner-lt1.xbm)"
  let wrap_style fg bg title = "^bg(" ++ bg ++ ")^fg(#000)" ++ icon
                              ++ dzenColor fg bg title
                              ++ "^bg(#000)^fg(" ++ bg ++ ")" ++ icon

  dynamicLogWithPP $ defaultPP
    { ppCurrent           =   wrap_style color_ppCurrent         color_bgCurrent . wrap " " " "
    , ppVisible           =   wrap_style color_ppVisible         color_bgInactive . pad
    , ppHidden            =   wrap_style color_ppHidden          color_bgInactive . check
    , ppHiddenNoWindows   =   wrap_style color_ppHiddenNoWindows color_bgInactive . pad
    , ppUrgent            =   wrap_style color_ppUrgent          color_bgInactive . pad
    , ppTitle             =   wrap_style color_ppTitle           color_BG . pad . dzenEscape
    , ppLayout            =   wrap_style color_ppLayout          color_BG .
                              (\x -> case x of
                                  "Tabbed ResizableTall"         ->  "^i(" ++ myBitmapsDir ++ "tall.xbm)"
                                  "Tabbed Mirror ResizableTall"  ->  "^i(" ++ myBitmapsDir ++ "mtall.xbm)"
                                  "Tabbed Full"                  ->  "^i(" ++ myBitmapsDir ++ "full.xbm)"
                                  "Tabbed Simple Float"          ->  "~"
                                  _                              ->  x
                              )
    , ppWsSep             =   ""
    , ppSep               =   dzenEscape "       "
    , ppOutput            =  (\str -> mapM_ (\handler -> hPutStrLn handler str) handlers)
    }
-- }}}


-- Manage Hook {{{
manageHook' :: ManageHook
manageHook' = (composeAll . concat $
    [ [resource     =? r     --> doIgnore         | r   <- myIgnores]
    , [className    =? c     --> doShift ws1      | c   <- myConsole]
    , [className    =? c     --> doShift ws2      | c   <- myVim    ]
    , [className    =? c     --> doShift ws3      | c   <- myInet   ]
    , [className    =? c     --> doShift ws4      | c   <- myChat   ]
    , [className    =? c     --> doShift ws5      | c   <- myMusic  ]
    , [className    =? c     --> doShift ws6      | c   <- myGimp   ]
    , [className    =? c     --> doShift ws9      | c   <- myExtra  ]
    , [className    =? c     --> doCenterFloat    | c   <- myFloats ]
    , [name         =? n     --> doCenterFloat    | n   <- myNames  ]
    , [isFullscreen          --> myDoFullFloat                      ]
    ])

    where

        role      = stringProperty "WM_WINDOW_ROLE"
        name      = stringProperty "WM_NAME"

        myNames   = []

        -- classNames
        myFloats  = ["SMPlayer","MPlayer","VirtualBox","Xmessage","XFontSel","Downloads","Nm-connection-editor", "Vlc", "Kodi"]
        myInet    = ["Firefox","Google-chrome","Chromium", "Chromium-browser"]
        myMusic   = ["Clementine"]
        myChat    = ["Skype"]
        myExtra   = ["Pidgin", "Buddy List", "HipChat"]
        myGimp    = ["Gimp"]
        myConsole = []
        myVim     = ["GVim"]
        myIgnores = ["desktop", "desktop_window", "notify-osd", "stalonetray", "trayer", "XXkb", "panel", "Conky"]

-- a trick for fullscreen but stil allow focusing of other WSs
myDoFullFloat :: ManageHook
myDoFullFloat = doF W.focusDown <+> doFullFloat
-- }}}


-- Layout Hook {{{
layoutHook'  =  onWorkspaces [ws1, ws5] customLayout $
                onWorkspaces [ws4, ws9] imLayout $
                customLayout

gaps' = gaps [(U,13), (D,13)]

customLayout =gaps'
                $ configurableNavigation noNavigateBorders
                $ addTabs shrinkText tabTheme
                $ subLayout [0,1] (Simplest)
                $ tiled
                  ||| Mirror tiled
                  ||| Full
                  ||| magnifier (Tall 1 (3/100) (1/2))

                where
                  tiled = ResizableTall 1 (2/100) (1/2) []

imLayout = gaps' $ ResizableTall 1 (5/100) (5/6) []
-- }}}


-- Colors {{{
tabTheme = defaultTheme { decoHeight = 14
                        , activeColor = "#303030"
                        , activeBorderColor = "#404040"
                        , activeTextColor = "#00af00"
                        , inactiveTextColor = "#8f8f8f"
                        , inactiveColor = "#252525"
                        , inactiveBorderColor = "#404040"
                        }

color_BG                = "#000000"
color_bgCurrent         = "#00af00"
color_bgInactive        = "#1b1d1e"

color_ppCurrent         = "#000"
color_ppVisible         = "#00af00"
color_ppHidden          = "#005800"
color_ppCopy            = "red"
color_ppHiddenNoWindows = "#7b7b7b"
color_ppUrgent          = "#ff0000"
color_ppLayout          = "#870000"
color_ppTitle           = "#7b7b7b"


colorNormalBorder       = "#1b1d1e"
colorFocusedBorder      = "#4e4e4e"
-- }}}


-- Key mapping {{{
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask,                    xK_p        ), spawn dmenuBar)
    , ((modMask .|. shiftMask,      xK_Return   ), spawn $ XMonad.terminal conf)
    , ((modMask .|. shiftMask,      xK_c        ), kill)                                        -- close window with all copy
    , ((modMask,                    xK_c        ), kill1)                                       -- close copy to the window

    , ((0,                          xK_Print    ), spawn "scrot -e 'mv $f ~/screenshots/'")
    , ((modMask,                    xK_o        ), spawn "firefox")
    , ((modMask,                    xK_m        ), spawn "thunar")
    , ((modMask .|. controlMask,    xK_Return   ), spawn "terminator")
    , ((modMask .|. shiftMask,      xK_l        ), spawn "slock")

    -- layouts
    , ((modMask,                    xK_space    ), sendMessage NextLayout)
    , ((modMask .|. shiftMask,      xK_space    ), setLayout $ XMonad.layoutHook conf)          -- reset layout on current desktop to default

    -- toggle both, Structs and Gaps
    , ((modMask,                    xK_b        ), do { sendMessage $ ToggleGaps; sendMessage ToggleStruts })
    , ((modMask,                    xK_n        ), refresh)

    , ((modMask,                    xK_Tab      ), windows W.focusDown)                         -- move focus to next window
    , ((modMask .|. shiftMask,      xK_Tab      ), windows W.focusUp  )                         -- move focus to the previous window
    , ((modMask,                    xK_j        ), windows W.focusDown)
    , ((modMask,                    xK_k        ), windows W.focusUp  )
    , ((modMask,                    xK_m        ), windows W.focusMaster)                       -- move focus to the master window

    , ((modMask .|. shiftMask,      xK_j        ), windows W.swapDown)                          -- swap the focused window with the next window
    , ((modMask .|. shiftMask,      xK_k        ), windows W.swapUp)                            -- swap the focused window with the previous window
    , ((modMask,                    xK_Return   ), windows W.swapMaster)
    , ((modMask,                    xK_t        ), withFocused $ windows . W.sink)              -- push window back into tiling
    , ((modMask,                    xK_h        ), sendMessage Shrink)                          -- shrink a master area
    , ((modMask,                    xK_l        ), sendMessage Expand)                          -- expand a master area
    , ((modMask,                    xK_comma    ), sendMessage (IncMasterN 1))
    , ((modMask,                    xK_period   ), sendMessage (IncMasterN (-1)))
    , ((modMask,                    xK_BackSpace), focusUrgent)
    , ((modMask,                    xK_a        ), sendMessage MirrorShrink)
    , ((modMask,                    xK_z        ), sendMessage MirrorExpand)

    , ((modMask .|. shiftMask,      xK_minus    ), sendMessage MagnifyMore)
    , ((modMask .|. controlMask,    xK_minus    ), sendMessage MagnifyLess)
    , ((modMask .|. controlMask,    xK_m        ), sendMessage Toggle)

    -- toggle visible window on all workspace
    , ((modMask,                    xK_v        ), windows copyToAll)
    , ((modMask .|. shiftMask,      xK_v        ), killAllOtherCopies)

    -- workspaces
    , ((modMask .|. controlMask,   xK_Right     ), nextWS)
    , ((modMask .|. shiftMask,     xK_Right     ), shiftToNext)
    , ((modMask .|. controlMask,   xK_Left      ), prevWS)
    , ((modMask .|. shiftMask,     xK_Left      ), shiftToPrev)

    , ((modMask,                   xK_r         ), swapPrevScreen)
    , ((modMask,                   xK_n         ), toggleWS)
    , ((modMask,                   xK_u         ), prevScreen)

    -- quit, or restart
    , ((modMask .|. shiftMask,      xK_q        ), io (exitWith ExitSuccess))
    , ((modMask,                    xK_q        ), restartHook >> restart "xmonad" True)
    , ((modMask,                    xK_g        ), goToSelected $ myGSConfig defaultColorizer)

    , ((modMask .|. controlMask,    xK_h        ), sendMessage $ pullGroup L)
    , ((modMask .|. controlMask,    xK_l        ), sendMessage $ pullGroup R)
    , ((modMask .|. controlMask,    xK_k        ), sendMessage $ pullGroup U)
    , ((modMask .|. controlMask,    xK_j        ), sendMessage $ pullGroup D)
    , ((modMask .|. controlMask,    xK_m        ), withFocused (sendMessage . MergeAll))
    , ((modMask .|. controlMask,    xK_u        ), withFocused (sendMessage . UnMerge))
    , ((modMask .|. controlMask,    xK_period   ), onGroup W.focusUp')
    , ((modMask .|. controlMask,    xK_comma    ), onGroup W.focusDown')

    , ((modMask,                    xK_Right    ), sendMessage $ Go R)
    , ((modMask,                    xK_Left     ), sendMessage $ Go L)
    , ((modMask,                    xK_Up       ), sendMessage $ Go U)
    , ((modMask,                    xK_Down     ), sendMessage $ Go D)

    -- FUNCTIONL KEYS
    , ((mod1Mask,                   xK_F1       ), spawn "suspend")
    , ((mod1Mask,                   xK_F2       ), spawn "light -s 10")
    , ((mod1Mask,                   xK_F3       ), spawn "light -a 10")
    , ((mod1Mask,                   xK_F4       ), spawn "sleep 1 && xset dpms force standby")

    -- Pulse audio
    , ((mod1Mask,                   xK_F7       ), spawn "amixer -D pulse set Master 0%")
    , ((mod1Mask,                   xK_F8       ), spawn "amixer -D pulse set Master 70%")
    , ((mod1Mask,                   xK_F9       ), spawn "amixer -D pulse set Master 100%")
    , ((mod1Mask,                   xK_Left     ), spawn "amixer -D pulse set Master 5%-")
    , ((mod1Mask,                   xK_Right    ), spawn "amixer -D pulse set Master 5%+")

    -- ALSA
    -- , ((mod1Mask,                xK_F7       ), spawn "amixer set Master 0%")
    -- , ((mod1Mask,                xK_F8       ), spawn "amixer set Master 70%")
    -- , ((mod1Mask,                xK_F9       ), spawn "amixer set Master 100%")
    -- , ((mod1Mask,                xK_Left     ), spawn "amixer set Master 5-")
    -- , ((mod1Mask,                xK_Right    ), spawn "amixer set Master 5+")

    , ((mod1Mask,                   xK_F10      ), spawn "ruby ~/.scripts/mpd.rb prev")
    , ((mod1Mask,                   xK_F11      ), spawn "ruby ~/.scripts/mpd.rb toggle")
    , ((mod1Mask,                   xK_F12      ), spawn "ruby ~/.scripts/mpd.rb next")
    ]

    ++
    -- mod-[1..9] %!                Switch to workspace N
    -- mod-shift-[1..9] %!          Move client to workspace N
    -- mod-control-shift-[1..9] %!  Copy client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]]

    ++
    -- mod-{w,e,r},         Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r},   Move client to screen 1, 2, or 3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
-- }}}


myGSConfig colorizer = (buildDefaultGSConfig colorizer) { gs_cellheight = 30, gs_cellwidth = 100 }
