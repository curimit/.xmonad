{-# LANGUAGE NoMonomorphismRestriction #-}
import XMonad

import Data.Monoid
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Util.Cursor
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Scratchpad
  
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.ShowWName
import XMonad.Layout.Minimize
  
import XMonad.Actions.Navigation2D
import XMonad.Actions.OnScreen
import XMonad.Actions.GridSelect
import XMonad.Actions.SwapWorkspaces

import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks

import XMonad.Config
import qualified XMonad.Prompt         as P
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Actions.Search as S


import XMonad.Actions.PhysicalScreens

import XMonad.Actions.WindowGo
import XMonad.Hooks.FadeWindows
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.CycleWS
import System.Exit

main = do
     xmonad $ defaultConfig
       { terminal          = myTerminal
       , modMask           = myModMask
       , borderWidth       = myBorderWidth
       , keys              = myKeys
       , mouseBindings     = myMouseBindings
       , layoutHook        = myLayout
       , workspaces        = myWorkspaces
       , focusFollowsMouse = True
       }

myTerminal = "gnome-terminal"
myModMask = mod4Mask
myBorderWidth = 3
myWorkspaces = (map (\x -> [x]) "`1234567890-=abcdefghijklmnopqrstuvwxyz;\"")
          
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList
    -- mod-button1 %! Set the window to floating mode and move by dragging
    [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w
                                          >> windows W.shiftMaster)
    -- mod-button2 %! Raise the window to the top of the stack
    , ((modMask, button2), windows . (W.shiftMaster .) . W.focusWindow)
    -- mod-button3 %! Set the window to floating mode and resize by dragging
    , ((modMask, button3), \w -> focus w >> mouseResizeWindow w
                                         >> windows W.shiftMaster)
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

myLayout = minimize ( tiled ||| Full )
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

myNavigation :: TwoD a (Maybe a)
myNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
               where navKeyMap = M.fromList [
                       ((0,xK_Escape), cancel)
                       ,((0,xK_Return), select)
                       ,((0,xK_slash) , substringSearch myNavigation >> select)
                       ,((0,xK_Left)  , move (-1,0)  >> myNavigation)
                       ,((0,xK_h)     , move (-1,0)  >> myNavigation)
                       ,((0,xK_Right) , move (1,0)   >> myNavigation)
                       ,((0,xK_l)     , move (1,0)   >> myNavigation)
                       ,((0,xK_Down)  , move (0,1)   >> myNavigation)
                       ,((0,xK_j)     , move (0,1)   >> myNavigation)
                       ,((0,xK_k)     , move (0,-1)  >> myNavigation)
                       ,((0,xK_Up)    , move (0,-1)  >> myNavigation)
                       ,((0,xK_y)     , move (-1,-1) >> myNavigation)
                       ,((0,xK_i)     , move (1,-1)  >> myNavigation)
                       ,((0,xK_n)     , move (-1,1)  >> myNavigation)
                       ,((0,xK_m)     , move (1,-1)  >> myNavigation)
                       ,((0,xK_space) , setPos (0,0) >> myNavigation)
                       ]
                     -- The navigation handler ignores unknown key symbols
                     navDefaultHandler = const myNavigation

gsconfig3 = defaultGSConfig
    { gs_cellheight = 80
    , gs_cellwidth = 200
    , gs_navigate = myNavigation
    }

myKeys = \conf -> mkKeymap conf $
       [("M-S-<Return>", spawn $ XMonad.terminal conf)
       ,("M-<Space>", sendMessage NextLayout)
       ,("M-S-q", io (exitWith ExitSuccess))
       ,("M-q", spawn "xmonad --recompile; xmonad --restart")

       -- ,("M-S-p", shiftToPrev)
       -- ,("M-S-n", shiftToNext)

       ,("M-S-t", withFocused $ windows . W.sink)
        
       ,("M-m", withFocused $ minimizeWindow)
       ,("M-S-m", sendMessage RestoreNextMinimizedWin)
          
        -- Directional navigation of windows
       ,("M-j", (windowGo U False))
       ,("M-k", (windowGo D False))
       
       ,("M-h", (windowGo L False))
       ,("M-l", (windowGo R False))

        -- Swap adjacent windows
       ,("M-S-j", (windowSwap U False))
       ,("M-S-k", (windowSwap D False))
       
       ,("M-S-h", (windowSwap L False))
       ,("M-S-l", (windowSwap R False))

       -- Swap workspaces on adjacent screens
       , ("M-t", screenSwap R False)
       
       -- ,("M-;", toggleWS)

       ,("M-S-c", kill)
       
       ,("M-,", sendMessage (IncMasterN 1))
       ,("M-.", sendMessage (IncMasterN (-1)))
       
       ,("M-<Left>", prevWS)
       ,("M-<Right>", nextWS)
        
       ,("M-g", goToSelected defaultGSConfig)
       ,("M-w", gridselectWorkspace defaultGSConfig (\ws -> W.greedyView ws))
       ,("M-p", spawnSelected defaultGSConfig ["chrome","emacs", "gnome-terminal"])
        
       ,("M-m", withFocused minimizeWindow)
       ,("M-S-m", withFocused minimizeWindow)

       ,("M-S-m", withFocused minimizeWindow)

       
       ,("<Scroll_lock>", spawn "gnome-screensaver-command -l")
       ,("M-o", spawn "gmrun")
       ]
       -- Search commands
       ++ [("M-/ " ++ k, S.promptSearch P.defaultXPConfig f) | (k,f) <- searchList ]
       ++ [("M-S-/ " ++ k, S.selectSearch f) | (k,f) <- searchList ]
       ++ [("M-1", windows (viewOnScreen 1 "1"))
          ,("M-2", windows (viewOnScreen 1 "2"))
          ,("M-3", windows (viewOnScreen 1 "3"))
          ,("M-4", windows (viewOnScreen 1 "4"))
          ,("M-5", windows (viewOnScreen 1 "5"))
          ,("M-6", windows (viewOnScreen 1 "6"))
          ,("M-7", windows (viewOnScreen 1 "7"))
          ,("M-8", windows (viewOnScreen 1 "8"))
          ,("M-9", windows (viewOnScreen 1 "9"))
          ,("M-0", windows (viewOnScreen 1 "0"))
          ,("M-`", windows (viewOnScreen 1 "`"))
          ,("M--", windows (viewOnScreen 1 "-"))
          ,("M-=", windows (viewOnScreen 1 "="))
          ,("M-; a", windows (viewOnScreen 1 "a"))
          ,("M-; s", windows (viewOnScreen 1 "s"))
          ,("M-; d", windows (viewOnScreen 1 "d"))
          ,("M-; f", windows (viewOnScreen 1 "f"))
          ]

       ++ [("M-C-1", windows (viewOnScreen 0 "1"))
          ,("M-C-2", windows (viewOnScreen 0 "2"))
          ,("M-C-3", windows (viewOnScreen 0 "3"))
          ,("M-C-4", windows (viewOnScreen 0 "4"))
          ,("M-C-5", windows (viewOnScreen 0 "5"))
          ,("M-C-6", windows (viewOnScreen 0 "6"))
          ,("M-C-7", windows (viewOnScreen 0 "7"))
          ,("M-C-8", windows (viewOnScreen 0 "8"))
          ,("M-C-9", windows (viewOnScreen 0 "9"))
          ,("M-C-0", windows (viewOnScreen 0 "0"))
          ,("M-C-`", windows (viewOnScreen 0 "`"))
          ,("M-C--", windows (viewOnScreen 0 "-"))
          ,("M-C-=", windows (viewOnScreen 0 "="))
          ,("M-C-; a", windows (viewOnScreen 0 "a"))
          ,("M-C-; s", windows (viewOnScreen 0 "s"))
          ,("M-C-; d", windows (viewOnScreen 0 "d"))
          ,("M-C-; f", windows (viewOnScreen 0 "f"))
          ]

       -- ++ [("M-C-1", windows (swapWithCurrent "1"))
       --    ,("M-C-2", windows (swapWithCurrent "2"))
       --    ,("M-C-3", windows (swapWithCurrent "3"))
       --    ,("M-C-4", windows (swapWithCurrent "4"))
       --    ,("M-C-5", windows (swapWithCurrent "5"))
       --    ,("M-C-6", windows (swapWithCurrent "6"))
       --    ,("M-C-7", windows (swapWithCurrent "7"))
       --    ,("M-C-8", windows (swapWithCurrent "8"))
       --    ,("M-C-9", windows (swapWithCurrent "9"))
       --    ,("M-C-0", windows (swapWithCurrent "0"))
       --    ,("M-C-`", windows (swapWithCurrent "`"))
       --    ,("M-C--", windows (swapWithCurrent "-"))
       --    ,("M-C-=", windows (swapWithCurrent "="))
       --    ,("M-C-; a", windows (swapWithCurrent "a"))
       --    ,("M-C-; s", windows (swapWithCurrent "s"))
       --    ,("M-C-; d", windows (swapWithCurrent "d"))
       --    ,("M-C-; f", windows (swapWithCurrent "f"))
       --    ]

       ++ [("M-S-1", windows (W.shift "1"))
          ,("M-S-2", windows (W.shift "2"))
          ,("M-S-3", windows (W.shift "3"))
          ,("M-S-4", windows (W.shift "4"))
          ,("M-S-5", windows (W.shift "5"))
          ,("M-S-6", windows (W.shift "6"))
          ,("M-S-7", windows (W.shift "7"))
          ,("M-S-8", windows (W.shift "8"))
          ,("M-S-9", windows (W.shift "9"))
          ,("M-S-0", windows (W.shift "0"))
          ,("M-S-`", windows (W.shift "`"))
          ,("M-S--", windows (W.shift "-"))
          ,("M-S-=", windows (W.shift "="))
          ,("M-S-; a", windows (W.shift "a"))
          ,("M-S-; s", windows (W.shift "s"))
          ,("M-S-; d", windows (W.shift "d"))
          ,("M-S-; f", windows (W.shift "f"))
          ]
       -- send to physical screen
       -- ++ [("M-S-1", sendToScreen 0)
       --    ,("M-S-2", sendToScreen 1)
          -- ]
       
       ++ [("M-<F12>", scratchpadSpawnActionCustom "xterm -name node -e node")]
 
searchList :: [(String, S.SearchEngine)]
searchList =  [ mk "h" "haskell-wiki" "http://www.haskell.org/hoogle/?q="
              , mk "g" "google" "http://www.google.com/search?num=100&q="
      	      , mk "w" "wikipedia" "http://en.wikipedia.org/wiki/Special:Search?go=Go&search="
      	      , mk "d" "dict-youdao" "http://dict.youdao.com/search?q="
      	      , mk "r" "ruby-doc" "http://www.ruby-doc.org/search.html?sa=Search&q="
      	      , mk "p" "python-doc" "http://docs.python.org/search.html?check_keywords=yes&area=default&q="
      	      , mk "s" "scholar-google" "https://scholar.google.de/scholar?q="
      	      , mk "a" "wolframalpha" "http://www.wolframalpha.com/input/?i="
              ]
           where
             mk = \key name url -> (key, S.searchEngine name url)


