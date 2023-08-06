{-# LANGUAGE ScopedTypeVariables #-}

module Config.Alias
  ( fnAwe
  , fnNerd
  ) where

import Prelude
import XMonad.Hooks.StatusBar.PP (wrap)

fnNerd :: String -> String
fnNerd = wrap "<fn=5>" "</fn>"

fnAwe :: String -> String
fnAwe = wrap "<fn=5>" "</fn>" . concatMap (: " ")
