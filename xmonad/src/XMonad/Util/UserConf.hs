{-# LANGUAGE DerivingVia #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.UserConf
-- Description :  Set up some config variables
-- Copyright   :  (c) Mislav Zanic 2023
-- License     :  BSD3-style
--
-- Maintainer  :  <mislavzanic3@gmail.com>
-- Stability   :  experimental
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module XMonad.Util.UserConf where

import XMonad hiding (terminal, borderWidth, modMask)
import XMonad.Actions.TopicSpace
import XMonad.Prompt (XPConfig)
import Config.Prompts (defaultPromptTheme)

import XMonad.Util.PTL

data UserConf = UserConf
  { userConfDir        :: String
  , userEditor         :: String
  , userBrowser        :: String
  , userTerminal       :: String
  , userLock           :: String
  , userFontStr        :: String
  , userBorderWidth    :: Dimension
  , userSpacingWidth   :: Integer
  , userModMask        :: KeyMask
  , userDefaultProfile :: String
  , userTopics         :: [ProfileTopicLayout]
  , userTopicConfig    :: TopicConfig
  , userPromptConfig   :: XPConfig
  }

instance Default UserConf where
  def = UserConf { userConfDir        = ""
                 , userEditor         = "vim"
                 , userBrowser        = "firefox"
                 , userTerminal       = "alacritty"
                 , userLock           = "i3lock"
                 , userFontStr        = "xft:Cantarell:size=8:antialias=true:autohint=false:style=bold"
                 , userBorderWidth    = 1
                 , userSpacingWidth   = 2
                 , userModMask        = mod1Mask
                 , userDefaultProfile = "Home"
                 , userTopics         = [genericTopic i | i <- map show [1..9 :: Int]]
                 , userTopicConfig    = def
                 , userPromptConfig   = defaultPromptTheme
                 }
                 where
                  genericTopic name = mkPTL ["Home"] ["tiled"] $ inHome name $ return ()
