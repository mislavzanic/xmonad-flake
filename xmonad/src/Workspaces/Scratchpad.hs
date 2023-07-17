module Workspaces.Scratchpad ( myScratchpads ) where

import XMonad
import XMonad.Util.Run

import XMonad.Util.NamedScratchpad ( customFloating
                                   , NamedScratchpad(NS)
                                   )

import qualified XMonad.StackSet as W

myScratchpads :: X [NamedScratchpad]
myScratchpads = do
  floatTerm <- getInput $ inTerm >-> setXClass termScratchpadName
  emacsScratch <- getInput $ inEditor >-> setFrameName scratchFrame >-> eval (elispFun "dashboard-refresh-buffer")

  pure [ NS "Term"    floatTerm    (appName =? termScratchpadName) floatMiddle
       , NS "Spotify" spotify      (appName =? spotify <||> title =? "") floatMiddle
       , NS "Emacs"   emacsScratch (appName =? scratchFrame)       floatMiddle
       ]
  where
    spotify = "spotify"
    termScratchpadName = "scratchpad"
    scratchFrame = "emacs-scratchpad"

    floatMiddle = customFloating $ W.RationalRect l t w h
    h = 0.9
    w = 0.9
    t = 0.95 - h
    l = 0.95 - w

