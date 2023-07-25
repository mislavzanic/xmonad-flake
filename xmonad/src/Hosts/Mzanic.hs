{-# LANGUAGE DerivingVia #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Hosts.Milivoj
-- Description :  Config for my work laptop
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
import XMonad.Actions.TopicSpace hiding ( switchNthLastFocusedByScreen, workspaceHistoryByScreen )
import XMonad.Util.Run
import Workspaces.Topics

defaultProfile :: String
defaultProfile = "Home"

topicConfig :: TopicConfig
topicConfig = def
  { topicDirs          = tiDirs    $ map topicItem topics
  , topicActions       = tiActions $ map topicItem topics
  , defaultTopicAction = const (pure ())
  , defaultTopic       = "web"
  }

topics :: [ProfileItem]
topics = homeTopics

homeTopics :: [ProfileItem]
homeTopics =
  [ mkItem ["Home"] ["tiled"] $ inHome "web" (spawn "google-chrome") ]
  <> map (mkItem ["Home"] ["hack"])
  [ TI "dots"  "~/dotfiles"                           $ spawnEditorInTopic topicConfig
  , TI "3"     "~" $ spawnTermInTopic topicConfig
  , TI "4"     "~" $ spawnTermInTopic topicConfig
  , TI "5"     "~" $ spawnTermInTopic topicConfig
  ]
