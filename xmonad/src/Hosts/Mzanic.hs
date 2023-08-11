{-# LANGUAGE DerivingVia #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Hosts.Mzanic
-- Description :  Config for a laptop
-- Copyright   :  (c) Mislav Zanic 2023
-- License     :  BSD3-style
--
-- Maintainer  :  <mislavzanic3@gmail.com>
-- Stability   :  experimental
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module Hosts.Mzanic where

import XMonad
import XMonad.Prompt
import XMonad.Util.UserConf
import XMonad.Actions.TopicSpace hiding ( switchNthLastFocusedByScreen, workspaceHistoryByScreen )
import Workspaces.Topics
import Config.Prompts (defaultPromptTheme)

userConf :: UserConf
userConf = def
  { userConfDir = "/home/mzanic/.config"
  , userEditor  = "emacsclient -c -a'emacs'"
  , userBrowser = browser
  , userLock    = "slock"
  , userModMask = mod4Mask
  , userTopics = home
  , userTopicConfig = topicConfig
  , userPromptConfig = promptTheme
  }
  where
   topicConfig :: TopicConfig
   topicConfig = def
     { topicDirs          = tiDirs    $ map topicItem home
     , topicActions       = tiActions $ map topicItem home
     , defaultTopicAction = const (pure ())
     , defaultTopic       = "1"
     }

   browser = "google-chrome"

   home :: [ProfileItem]
   home =
     [ mkItem ["Home"] ["tiled"] $ inHome "1" (spawn $ browser <> " --profile-directory=work")
     , mkItem ["Home"] ["tiled"] $ inHome "2" (spawn $ browser <> " --profile-directory=PM")
     ]
     <>
     [ mkItem ["Home"] ["hack"] (genericTopic i) | i <- map show [3..8 :: Int] ]
     <>
     [ mkItem ["Home"] ["tiled"] $ inHome "9" (spawn $ browser <> " --profile-directory=personal")]
    where
      genericTopic name = inHome name $ spawnTermInTopic topicConfig

   promptTheme :: XPConfig
   promptTheme = defaultPromptTheme 
     { height = 32
     , font = userFontStr def
     }
