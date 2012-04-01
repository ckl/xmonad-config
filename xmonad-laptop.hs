-- My XMonad configuration file
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.WindowGo	
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.IM
import XMonad.Layout.Maximize
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.Run
import XMonad.Util.Scratchpad

import qualified Data.Map as M
import qualified XMonad.Layout.Magnifier as Mag
import qualified XMonad.StackSet as W
import System.IO

import MyScratchpads

-- Default terminal. Called with mod+shift+enter
myTerminal = "lxterminal"

-- Border width around [in]active windows
myBorderWidth = 2

-- Border colors for focused and unfocused windows
myNormalBorderColor    = "#cccccc"
myFocusedBorderColor   = "#00a2ff"

-- Default mod mask, mod1Mask = alt
myModMask = mod1Mask

-- Whether or not focus follows the mouse pointer
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- List of workspace names
myWorkspaces = 
	[ "1:web"
	, "2:code"
	, "3:code"
	, "4:"
	, "5:"
	, "6:"
	, "7:gimp"
	, "8:aard"
	, "9:bt"
	]


-- Default XPConfig color theme used by shellPrompt and windowPromptGoto
myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
    { font = "-*-terminus-*-r-normal-*-*-120-*-*-*-*-iso8859-*"
    , bgColor = "black"
    , fgColor = "#59bbe8"
    , fgHLight = "black"
    , bgHLight = "#59bbe8"
    , borderColor = "#59bbe8" }

-----------------------------------------------------------
-- Programs to run at startup
-----------------------------------------------------------
myStartup :: X ()
myStartup = do
	spawn "nitrogen --restore"
    -- only run transmission if it's not already running
	runOrRaise "transmission" (className =? "transmission")

-----------------------------------------------------------
-- Window and scratchpad hooks
-----------------------------------------------------------
-- 
myWindowHook = (composeAll . concat $
    [ [(role =? "gimp-toolbox" <||> role =? "gimp-image-window" <||> role =? "gimp-dock") --> (ask >>= doF . W.sink)]
    , [ className =? x --> doF (W.shift "7:gimp")   | x <- myGimpApp ]
    , [ className =? x --> doF (W.shift "8:aard")   | x <- myAardApp ]
    , [ className =? x --> doF (W.shift "9:bt")     | x <- myBtApp ]
    , [ className =? x --> doCenterFloat            | x <- myCenterFloats ]
    , [ isFullscreen   --> doFullFloat]
    ]) 
    where
        role           = stringProperty "WM_WINDOW_ROLE" 
        myGimpApp      = ["Gimp", "Gimp-2.6"]
        myAardApp      = ["Wine"]
        myBtApp        = ["Transmission"]
        myCenterFloats = ["File-roller"]

-- scratchpad terminal hook called when pressing F12
terminalScratchpad :: ManageHook
terminalScratchpad = scratchpadManageHook (W.RationalRect l t w h)
    where
        h = 0.50    -- height percent
        w = 0.50    -- width  percent
        t = 0.01    -- distance from top  (percent)
        l = 0.01    -- distance from left (percent)

-- create scratchpads using definitions from MyScratchpad.hs
myScratchpads :: [Scratchpad]
myScratchpads =  [cmus, htop, termScratchpad]

cmus :: Scratchpad
cmus = makeTerminalScratchpad (mod4Mask, xK_m) "cmus" $ centerScreen 0.5 0.5

htop :: Scratchpad
htop = makeTerminalScratchpad (mod4Mask, xK_h) "htop" $ bottomCenter 0.7 0.4

termScratchpad :: Scratchpad
termScratchpad = makeTerminalScratchpad (0, xK_F12) "bash" $ topLeft 0.5 0.5

-- combine all of our hooks and call it 'myManageHook'
myManageHook = manageHook defaultConfig <+>
               manageDocks              <+>
               myWindowHook             <+> 
               terminalScratchpad       <+> getScratchpadHooks myScratchpads

-----------------------------------------------------------
-- Layouts
-----------------------------------------------------------
myLayout = onWorkspace "7:gimp" (named "Gimp" gimpFirst)   $ 
           named "myTall"       (modifiers tiled)          |||
           named "myFull"       (noBorders Full)           -- |||
           -- I find myself almost never using this layout
           --named "myMirror"     (modifiers $ Mirror tiled) 
    where 
        gimpFirst   = gimpLayout ||| modifiers tiled ||| modifiers (Mirror tiled)
        tiled       = ResizableTall nmaster delta ratio []
        modifiers x = maximize $ avoidStruts $ Mag.magnifiercz 1.0 x
        --modifiers x = spacing (7) $ maximize $ avoidStruts $ Mag.magnifiercz 1.0 x
        -- gimp toolbar is on the left, docks on the right, image in middle
        gimpLayout  = avoidStruts  $ withIM (0.15) (Role "gimp-toolbox") $
                      reflectHoriz $ withIM (0.2)  (Role "gimp-dock") 
                      (ResizableTall 3 delta ratio [])
        nmaster     = 1
        delta       = 3/100
        ratio       = 1/2

-----------------------------------------------------------
-- logHooks and status bars
-----------------------------------------------------------

-- Top left status bar and top right conky bar
myDzenBar  = "dzen2 -h 18 -w 700 -ta l "-- ++ myBarFont
myConkyBar = "conky -c ~/.xmonad/conkyrc | dzen2 -h 18 -w 930 -x 700 -ta r"-- ++ myBarFont
myBarFont  = " -fn '-*-terminus-*-r-normal-*-*-120-*-*-*-*-iso8859-*'"
myTrayer   = "trayer --edge top --align right --widthtype pixel --width 50 -- heighttype pixel --height 18"

-- Colors used for left status bar
myFontColor = "#ffffff"

-- Format is dzenColor <foreground> <background>
myPPCurrentColor = dzenColor "#1a1a1a" "#00a2ff"
myPPVisibleColor = dzenColor "#59bbe8" "gray30"
myPPHiddenColor  = dzenColor myFontColor ""
myPPTitleColor   = dzenColor myFontColor ""
myPPLayoutColor  = dzenColor "#59bbe8" ""

myIconPath   = "/home/chris/.xmonad/images/icons/"
myMarkerIcon = "^i(" ++ myIconPath ++ "marker.xbm)"

myDzenPP h = dzenPP
    {
          ppCurrent = myPPCurrentColor . wrap ("["++myMarkerIcon) "]" 
        , ppVisible = myPPVisibleColor . wrap ("["++myMarkerIcon) "]"
        , ppHidden  = myPPHiddenColor  . wrap "[" "]" . noScratchpad
        , ppTitle   = myPPTitleColor   . pad . shorten 50
        , ppWsSep   = " "
        , ppSep     = " | "
        , ppOutput  = hPutStrLn h
        , ppLayout  = myPPLayoutColor . 
            (\x -> case x of
                "myTall"   -> "Tall"        ++ insertLayoutIcon "tall"
                "myMirror" -> "Mirror Tall" ++ insertLayoutIcon "mtall"
                "myFull"   -> "Full"        ++ insertLayoutIcon "full"
                _ -> x
            )
    }
    where
        insertLayoutIcon :: String -> String
        insertLayoutIcon icon = " " ++ (getLayoutIcon icon)
        getLayoutIcon    :: String -> String
        getLayoutIcon icon = "^i(" ++ myIconPath ++ "layout-" ++ icon ++ ".xbm)"
        noScratchpad ws = if ws == "NSP" then "" else ws


myUrgencyHook = withUrgencyHook dzenUrgencyHook
    { args = ["-bg", "darkgreeen"] } 

-----------------------------------------------------------
-- Key bindings
-----------------------------------------------------------
myKeys = M.fromList $
        [ 
          -----------------------------------------------------------
          -- XMonad UI keys
          -----------------------------------------------------------
          -- toggle panel visibility (doesn't work properly with dzen2)
          --((myModMask, xK_b), sendMessage ToggleStruts)
          -- kill conky and dzen2 to hide. restart xmonad to show again
          ((myModMask, xK_b),  spawn "killall conky && killall dzen2")
          -- ctrl+q closes active window
        , ((controlMask, xK_q), kill)                 
          -- expand tiled window vertically (for ResizableTall layout)
        , ((myModMask, xK_o), sendMessage MirrorExpand)
          -- shrink tiled window vertically (for ResizableTall layout)
	    , ((myModMask, xK_u), sendMessage MirrorShrink) 
          -- pops open a 'go to window' prompt
	    , ((mod4Mask, xK_g), windowPromptGoto myXPConfig) 
          -- increase window magnification
        , ((myModMask, xK_equal), sendMessage Mag.MagnifyMore) 
          -- decrease window magnification
        , ((myModMask, xK_minus), sendMessage Mag.MagnifyLess)
          -- toggle active window magnifier on/off (grave is `)
        , ((myModMask, xK_bracketleft), sendMessage Mag.Toggle)
          -- toggle maximize window
        , ((myModMask, xK_grave), withFocused (sendMessage . maximizeRestore))
          -- open xmonad shell to launch applications
        , ((myModMask, xK_p), shellPrompt myXPConfig)
          -- open the scratchpad terminal
        , ((0, xK_F11), terminalScratchpadKey)
          -----------------------------------------------------------
          -- Program hotkeys
          -----------------------------------------------------------
        , ((mod4Mask, xK_a), spawn "lxterminal")
        , ((mod4Mask, xK_f), spawn "pcmanfm")
        , ((mod4Mask, xK_x), spawn "chromium-browser")
        , ((mod4Mask, xK_0), spawn "chromium-browser --incognito")
        , ((mod4Mask, xK_t), spawn "transmission")
        , ((mod4Mask, xK_c), spawn "pcmanfm")
          -----------------------------------------------------------
	      -- multimedia hotkeys
          -----------------------------------------------------------
        , ((mod1Mask,  xK_z),    spawn "cmus-remote --pause")
        , ((mod1Mask,  xK_a),    spawn "cmus-remote --prev")
        , ((mod1Mask,  xK_s),    spawn "cmus-remote --next")
        , ((mod1Mask,  xK_d),    spawn "cmus-remote --stop")
        , ((myModMask, xK_Up),   spawn "amixer set Master 1+")
        , ((myModMask, xK_Down), spawn "amixer set Master 1-")
          -----------------------------------------------------------
          -- folder hotkeys
          -----------------------------------------------------------
        , ((controlMask .|. shiftMask, xK_r), spawn "pcmanfm /")
        , ((controlMask .|. shiftMask, xK_d), spawn "pcmanfm /media/media/bt")
        , ((controlMask .|. shiftMask, xK_a), spawn "pcmanfm /media/media/Anime")
        , ((controlMask .|. shiftMask, xK_z), spawn "pcmanfm /media/media/Music")
        , ((controlMask .|. shiftMask, xK_x), spawn "pcmanfm /media/media/Movies")
        , ((controlMask .|. shiftMask, xK_s), spawn "pcmanfm /media/media/tvshows")
        , ((controlMask .|. shiftMask, xK_c), spawn "pcmanfm /media/documents/documents")
        , ((controlMask .|. shiftMask, xK_e), spawn "pcmanfm /media/documents/documents/ebooks")
          -----------------------------------------------------------
	      -- miscellaneous hotkeys
          -----------------------------------------------------------
          -- myModMask+shift+l locks the screen
        , ((myModMask.|. shiftMask, xK_l), spawn "xscreensaver-command -lock")
          -- alt+print-screen takes screenshot of next window to be clicked
        , ((mod1Mask, xK_Print), spawn "sleep 0.2; scrot -s")
          -- printscreen takes a screenshot of the entire screen
        , ((0, xK_Print), spawn "scrot")
          -----------------------------------------------------------
        ]  ++ getScratchpadKeys myScratchpads
        -- must set these flags manually to use lxterminal with scratchpad
        where 
            terminalScratchpadKey = scratchpadSpawnActionCustom 
                                    "lxterminal --title=scratchpad"
                                    --"lxterminal --disable-factory --name scratchpad"

-----------------------------------------------------------
-- Mouse bindings
-----------------------------------------------------------

myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X())
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ 
      -- mod-button1 %! Set the window to floating mode and move by dragging
      ((modMask, button1), (\w -> focus w >> mouseMoveWindow w
                                          >> windows W.shiftMaster))
      -- mod-button2 %! Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.shiftMaster))
      -- mod-button3 %! Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w
                                            >> windows W.shiftMaster))
      -- hold control + alt and click on a window to swap it with the master
    , ((controlMask .|. mod1Mask, button1), (\w -> focus w >> windows W.swapMaster))
    ]

-----------------------------------------------------------

main = do
    xmproc <- spawnPipe myDzenBar
    conky  <- spawnPipe myConkyBar
    xmonad $ myUrgencyHook $ defaultConfig
        { 
          -- basic settings
          focusFollowsMouse  = myFocusFollowsMouse
	    , workspaces         = myWorkspaces
	    , borderWidth        = myBorderWidth
        , terminal           = myTerminal
        , modMask            = myModMask
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor

          -- layouts and hooks 
        , startupHook = myStartup
        , layoutHook  = myLayout
        , logHook     = dynamicLogWithPP $ myDzenPP xmproc 
--        , manageHook  = myManageHook <+> manageHook defaultConfig
        , manageHook = myManageHook

          -- key/mouse bindings
        , keys          = \c -> myKeys `M.union` keys defaultConfig c
        , mouseBindings = myMouseBindings
        } 


--------------------------
-- Old loghook:
--------------------------
-- xmproc <- spawnPipe "xmobar"
-- logHook = dynamicLogWithPP $ xmobarPP
-- { ppOutput = hPutStrLn xmproc
-- , ppTitle = xmobarColor "green" "" . shorten 50
-- }
