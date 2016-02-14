-- My XMonad configuration

import XMonad

import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Shell

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops

import XMonad.Util.Run
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS

import XMonad.Layout.NoBorders
import XMonad.Layout.Minimize

import Data.Bits ((.|.))
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map as M


-- Key bindings
myKeys conf@(XConfig {XMonad.modMask = modMask}) = keymap
    where
      keymap = M.fromList $ [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
                            , ((modMask .|. shiftMask, xK_semicolon), lockScreen)
                            -- minimize and restore
                            , ((modMask              , xK_n), withFocused minimizeWindow)
                            , ((modMask .|. shiftMask, xK_n), sendMessage RestoreNextMinimizedWin)
                            -- convenient workspace navigation
                            , ((modMask              , xK_comma), prevWS)
                            , ((modMask .|. shiftMask, xK_comma), shiftToPrev)
                            , ((modMask              , xK_period), nextWS)
                            , ((modMask .|. shiftMask, xK_period), shiftToNext)
                            -- show calender
                            , ((modMask              , xK_c), showCal)
                            -- dictionary
                            , ((modMask              , xK_d), defineWord)
                            -- show lyrics
                            , ((modMask              , xK_i), showLyrics)
                            -- volume
                            , ((modMask              , xK_equal), raiseVolume)
                            , ((modMask              , xK_minus), lowerVolume)
                            -- program launcher
                            , ((modMask              , xK_p), shellPrompt myXPConfig)
                            -- flip touchpad state
                            , ((modMask              , xK_f), spawn "tflip")
                            , ((modMask              , xK_u), sendMessage ToggleStruts)
                            , ((modMask              , xK_s), spawn "scrot")
                            , ((modMask              , xK_Tab), cycleRecentWS [xK_Alt_L] xK_Tab xK_grave)
                            ]


-- Layout
myLayoutHook = avoidStruts . smartBorders $ vanillaLayout 
    where
      tiled = Tall nmaster delta ratio
      -- # of windows in master pane
      nmaster = 1
      -- fraction of screen occupied by master pane
      ratio = 1/2
      -- least-count for resize operations
      delta = 3/100
      -- Old vanilla layout
      vanillaLayout = (minimize tiled ||| Full ||| Mirror tiled)

-- Workspaces | logHook
myLogHook h = dynamicLogWithPP $ defaultPP
              { ppCurrent = dzenColor "#000000" "#809A06" . pad
              , ppHidden = dzenColor "#F57900" "#000000"
              , ppHiddenNoWindows = dzenColor "#909090" "#000000"
              , ppLayout = \_ -> " "
              , ppUrgent = dzenColor "#ff0000" "" . pad . dzenStrip
              , ppWsSep = "  |  "
              , ppSep = " "
              , ppTitle = ("^fg(lightblue)" ++) . dzenEscape
              , ppOutput = hPutStrLn h
              }

-- manageHook
myManageHook = composeAll
               [ className =? "Guake" --> doFloat 
               , resource =? "MyEmacs" --> doShift beta
               , resource =? "MyWeechat" --> doShift gamma
               ]
  where beta = myWorkspaces !! 1
        gamma = myWorkspaces !! 2
        
-- on Xmonad startup
myStartupHook :: X ()
myStartupHook =  do setWMName "LG3D" -- workaround for some applications

-- The main function
main = do
  -- spawn the status bars
  d <- spawnPipe $ "dzen2 -p -ta l -e 'onstart=lower' -fn " ++ myPanelFont
  spawn $ "i3status | dzen2 -ta r -x 999 -p -e 'onstart=raise' -fn " ++ myPanelFont
  -- startup applications
  bootstrapApps 
  -- xmonad
  xmonad $ withUrgencyHook dzenUrgencyHook { args = ["-bg", "black", "-fg", "white"] }
         $ ewmh defaultConfig
             { terminal = myTerminal
             , modMask = mod4Mask
             , workspaces = myWorkspaces
             , normalBorderColor = myNormalBorderColor
             , focusedBorderColor = myFocusedBorderColor
             , borderWidth = myBorderWidth
             , focusFollowsMouse = myFocusFollowsMouse
             -- layout
             , layoutHook = myLayoutHook
             -- bindings
             , keys = myKeys <+> keys defaultConfig
             -- startup
             , startupHook = myStartupHook
             -- logHook
             , logHook = myLogHook d
             -- manageHook
             , manageHook = myManageHook <+> manageHook defaultConfig
             -- hoping to get chromium get the focus change rigt
             , handleEventHook =
               handleEventHook defaultConfig <+> fullscreenEventHook
             }
             

--------------------------------------------------------------------------------
-- Functions and variables
--------------------------------------------------------------------------------
-- general [colors|border|terminal]
myTerminal = "urxvt -e tmux"
myEditor = "emacs"
myModMask = mod4Mask
myWorkspaces = ["δ", "β", "γ"]
myNormalBorderColor = "grey9"
myFocusedBorderColor = "#ee5c42"
myBorderWidth = 3
myFocusFollowsMouse = True


-- fonts
myNormalFont = "'Comic Sans MS:size=11'"
myPanelFont ="'Comic Sans MS:size=11'"
myMonoFont = "'Monaco:size=10'"

-- style the input prompt
myXPConfig = greenXPConfig { height = 21
                           , font = "xft: Monaco-10"
                           , position = Top
                           , showCompletionOnTab = True
                           , historyFilter = deleteConsecutive
                           }

-- Setup intial applications | Not using now.
bootstrapApps :: MonadIO m => m ()
bootstrapApps = do spawn "emacs --name MyEmacs"
                   spawn "sleep 5s && tflip"  -- I am going crazy as to why this isn't working!
show_x :: String
show_x = "~/dotfiles/local/bin/show_x "

-- Modify this to change the dictionary and lyrics views
pipeDzen align command = spawn $ command ++ " | dzen2 -sa " ++ align ++ " -l 20 -p -fn " ++ myNormalFont ++
                 " -e 'onstart=raise,grabkeys,uncollapse,scrollhome;" ++
                 "enterslave=grabkeys;key_j=scrolldown;key_k=scrollup;" ++
                 "button1=exit;key_q=ungrabkeys,exit;'"

-- start xscreensaver
lockScreen :: X ()
lockScreen = spawn "xscreensaver-command -lock"

-- show i3status bar
showStatusBar :: X ()
showStatusBar = spawn "i3status | dzen2 -ta c -p"

-- hide i3status bar | kill the i3status's process group
hideStatusBar :: X ()
hideStatusBar = spawn "pkill dzen2"

-- show lyrics for the song now being played on mpd
showLyrics :: X ()
showLyrics = pipeDzen "c" show_x

-- show calender
showCal :: X ()
showCal = spawn ("zenity --calendar")

-- define a word
-- prepend the word with a space to look it up in the computing dictionary!
-- prepend the word with '#' to look up in all available dictionaries
defineWord :: X ()
defineWord = inputPrompt myXPConfig "Word: " ?+ showDef
    where showDef word =
              case word of
                (' ':keyword) -> pipeDzen "l" (show_x ++ "'" ++ keyword ++ "'" ++ " 1 ")
                ('#':keyword) -> pipeDzen "l" (show_x ++ "'" ++ keyword ++ "'" ++ " 2 ")
                _ -> pipeDzen "l" (show_x ++ "'" ++ word ++ "'" ++ " 0 ")

-- raise volume
raiseVolume :: X ()
raiseVolume = changeVolume "increase"

-- lower volume
lowerVolume :: X ()
lowerVolume = changeVolume "decrease"

-- change volume
changeVolume :: String -> X ()
changeVolume action = let doAction = spawn ("echo '^fg(blue) Volume: ^fg(violet)' " ++
                                            "$(ponymix " ++ action ++ " 5)% | dzen2 -p 1")
                      in
                        case action of
                          "increase" -> doAction
                          "decrease" -> doAction
                          _          -> return ()

-- change opacity of the currently focused window
changeCWOpacity :: X ()
changeCWOpacity = error "To be defined."

-- start a new terminal or bring the already running one to focus #TODO
startTerminal :: X ()
startTerminal = error "To be defined."
