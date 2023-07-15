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

module Hosts.Milivoj where

import XMonad
import XMonad.Actions.TopicSpace hiding ( switchNthLastFocusedByScreen, workspaceHistoryByScreen )
import XMonad.Util.Run
import Workspaces.Topics

defaultProfile :: String
defaultProfile = "Work"

topicConfig :: TopicConfig
topicConfig = def
  { topicDirs          = tiDirs    $ map topicItem topics
  , topicActions       = tiActions $ map topicItem topics
  , defaultTopicAction = const (pure ())
  , defaultTopic       = "temp1"
  }

topics :: [ProfileItem]
topics = homeTopics <> workTopics <> notesTopics

homeTopics :: [ProfileItem]
homeTopics =
  [ mkItem ["Home"] ["tiled"] $ inHome "web" (spawn "firefox") ]
  <> map (mkItem ["Home"] ["hack"])
  [ TI "dots"  "~/.config/.dotfiles"                  $ spawnEditorInTopic topicConfig
  , TI "xmn"   "~/.config/.dotfiles/config/xmonad"    $ spawnEditorInTopic topicConfig
  , TI "emacs" "~/.config/.dotfiles/config/emacs"     $ spawnEditorInTopic topicConfig
  , TI "xmb"   "~/.config/.dotfiles/config/xmobar"    $ spawnEditorInTopic topicConfig
  , TI "nix-dev" "~/.local/dev/nix-tinkering" (spawnEditorInTopic topicConfig >> spawnTermInTopic topicConfig)
  ]

workTopics :: [ProfileItem]
workTopics = map (mkItem ["Work"] ["tiled"])
  [ inHome "brave"                              ( spawn "brave" )
  , inHome "slack"                              ( spawn "slack" )
  , TI     "k9s"   "~/.local/work/repos/argoCD" ( proc $ termInDir >-$ (currentTopicDir topicConfig <> pure (" --command " <> nixShellCommand)) )
  ] <> map (mkItem ["Work"] ["hack"])
  [ TI     "work"  "~/.local/work"              $ spawnEditorInTopic topicConfig
  , TI     "tfm"   "~/.local/work/repos/tf"     spawnProgs
  , TI     "helm"  "~/.local/work/repos/argoCD" $ spawnEditorInTopic topicConfig
  ]
  where
    spawnProgs :: X ()
    spawnProgs = spawnTermInTopic topicConfig *> spawnEditorInTopic topicConfig

    nixShellCommand :: String
    nixShellCommand  = "nix-shell -p k9s -p \"(google-cloud-sdk.withExtraComponents [google-cloud-sdk.components.kubectl])\" --command \"k9s -c ctx\""

notesTopics :: [ProfileItem]
notesTopics = map (mkItem ["Home", "Work"] ["tiled", "hack"])
  [ TI "notes" "~/.local/notes" $ spawnEditorInTopic topicConfig
  , inHome "temp1" $ spawnTermInTopic topicConfig
  ]
