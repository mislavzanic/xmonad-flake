{-# LANGUAGE DerivingVia #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.DebugWindow
-- Description :  Utility for debuging
-- Copyright   :  (c) Mislav Zanic 2023
-- License     :  BSD3-style
--
-- Maintainer  :  <mislavzanic3@gmail.com>
-- Stability   :  experimental
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module XMonad.Util.DebugWindow
  ( DebugWindow(..)
  , createDebugWindow
  , killDebugWindow
  )where

import XMonad
import XMonad.Util.ExtensibleState as XS
import XMonad.Util.XUtils

newtype DebugWindow = DebugWindow { window :: Window }

instance ExtensionClass DebugWindow where
  initialValue = DebugWindow 0

createDebugWindow :: WindowConfig -> [String] -> X()
createDebugWindow conf text = showSimpleWindow conf text >>= storeWindow

storeWindow :: Window -> X()
storeWindow w = XS.modify update
  where
   update :: DebugWindow -> DebugWindow
   update dw = dw { window = w }

killDebugWindow :: X()
killDebugWindow = XS.gets window >>= deleteWindow >> XS.modify update
  where
   update :: DebugWindow -> DebugWindow
   update dw = dw { window = 0 }
