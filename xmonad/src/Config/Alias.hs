{-# LANGUAGE ScopedTypeVariables #-}

module Config.Alias
  ( myModMask
  , myTerminal
  , configDir
  , myEditor
  , myPDF
  , myBrowser
  , myFont
  , myBorderWidth
  , spacingWidth
  , wsType
  , fnAwe
  , fnNerd
  ) where

import XMonad ( mod4Mask
              , Dimension
              , KeyMask
              )

import Prelude
import XMonad.Actions.Search ( Browser )
import XMonad.Hooks.StatusBar.PP (wrap)

configDir     :: String    = "/home/mzanic/.config"
myModMask     :: KeyMask   = mod4Mask
myTerminal    :: String    = "alacritty"
myEditor      :: String    = "emacsclient -c -a'emacs'"
myPDF         :: String    = "zathura"
myBrowser     :: Browser   = "firefox"
myFont        :: String    = "xft:Cantarell:size=8:antialias=true:autohint=false:style=bold"
myBorderWidth :: Dimension = 1
spacingWidth  :: Integer   = 2
wsType        :: String    = "topic"


fnNerd :: String -> String
fnNerd = wrap "<fn=5>" "</fn>"

fnAwe :: String -> String
fnAwe = wrap "<fn=5>" "</fn>" . concatMap (: " ")
