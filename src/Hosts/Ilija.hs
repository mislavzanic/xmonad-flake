{-# LANGUAGE DerivingVia #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Hosts.Ilija
-- Description :  Config for my personal laptop
-- Copyright   :  (c) Mislav Zanic 2023
-- License     :  BSD3-style
--
-- Maintainer  :  <mislavzanic3@gmail.com>
-- Stability   :  experimental
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module Hosts.Ilija where

import Config.Alias ( myEditor )

import XMonad
import XMonad.Actions.TopicSpace hiding ( switchNthLastFocusedByScreen, workspaceHistoryByScreen )
import Workspaces.Topics

defaultProfile :: String
defaultProfile = "Home"

topicConfig :: TopicConfig
topicConfig = def
  { topicDirs          = tiDirs    $ map topicItem topics
  , topicActions       = tiActions $ map topicItem topics
  , defaultTopicAction = const (pure ())
  , defaultTopic       = "temp1"
  }

topics :: [ProfileItem]
topics = homeTopics <> notesTopics

homeTopics :: [ProfileItem]
homeTopics =
  [ mkItem ["Dev", "Media", "Home"] ["tiled"] $ inHome "web" (spawn "firefox")
  , mkItem ["Dev"]                  ["hack"]  $ TI "dev" "~/.local/dev" $ spawnEditorInTopic topicConfig
  , mkItem ["Dev"]                  ["hack"]  $ TI "nix-dev" "~/.local/dev/nix-tinkering" (spawnEditorInTopic topicConfig >> spawnTermInTopic topicConfig)
  , mkItem ["Dev"]                  ["hack"]  $ TI "cp" "~/.local/dev/compprog" $ spawnEditorInTopic topicConfig
  ] <> map (mkItem ["Media"] ["tiled"])
  [ inHome "rss" (spawn $ myEditor <> " --eval '(elfeed)'")
  , TI "blog" "~/.local/dev/blog/webContent" $ spawnEditorInTopic topicConfig
  , TI "vid"  "~/.local/torrents" $ spawnTermInTopic topicConfig
  , TI "pdf"  "~/.local/books" $ spawnTermInTopic topicConfig
  ] <> map (mkItem ["Home"] ["hack"])
  [ TI "dots"  "~/.config/.dotfiles"                  $ spawnEditorInTopic topicConfig 
  , TI "xmn"   "~/.config/.dotfiles/config/xmonad"    $ spawnEditorInTopic topicConfig
  , TI "emacs" "~/.config/.dotfiles/config/emacs"     $ spawnEditorInTopic topicConfig
  , TI "xmb"   "~/.config/.dotfiles/config/xmobar"    $ spawnEditorInTopic topicConfig
  ]

notesTopics :: [ProfileItem]
notesTopics = map (mkItem ["Dev", "Home", "Media"] ["tiled", "hack"])
  [ TI "notes" "~/.local/notes" $ spawnEditorInTopic topicConfig
  , inHome "temp1" $ spawnTermInTopic topicConfig
  ]
