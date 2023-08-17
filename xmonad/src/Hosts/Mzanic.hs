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
import XMonad.Util.PTL
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
  , userDefaultProfile = "Work"
  }
  where
   topicConfig :: TopicConfig
   topicConfig = def
     { topicDirs          = tiDirs    $ map topicItem home
     , topicActions       = tiActions $ map topicItem home
     , defaultTopicAction = const (pure ())
     , defaultTopic       = "web-g"
     }

   browser = "google-chrome"

   home :: [ProfileTopicLayout]
   home =
     [ mkPTL ["Work"] ["tiled"] $ inHome "wwork" (spawn $ browser <> " --profile-directory=work")
     , mkPTL ["Work"] ["tiled"] $ inHome "wpm" (spawn $ browser <> " --profile-directory=PM")
     ]
     <>
     [ mkPTL ["Work"] ["hack"] (genericTopic i) | i <- map show [3..9 :: Int] ]
     <>
     [ mkPTL ["Home"] ["tiled"] $ inHome "wme" (spawn $ browser <> " --profile-directory=personal")
     , mkPTL ["Home"] ["hack"]  $ TI "dots"      "~/dotfiles"         $ spawnEditorInTopic topicConfig
     , mkPTL ["Home"] ["hack"]  $ TI "comp-prog" "~/.local/comp-prog" $ spawnEditorInTopic topicConfig
     , mkPTL ["Home"] ["hack"]  $ TI "dev"       "~/.local/dev"       $ spawnEditorInTopic topicConfig
     ]
    where
      genericTopic name = inHome name $ spawnTermInTopic topicConfig

   promptTheme :: XPConfig
   promptTheme = defaultPromptTheme 
     { height = 32
     , font = userFontStr def
     }
