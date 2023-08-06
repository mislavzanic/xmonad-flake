{-# LANGUAGE DerivingVia #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Hosts.Default
-- Description :  Config for my personal laptop
-- Copyright   :  (c) Mislav Zanic 2023
-- License     :  BSD3-style
--
-- Maintainer  :  <mislavzanic3@gmail.com>
-- Stability   :  experimental
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module Hosts.Default where

import XMonad
import XMonad.Prompt
import XMonad.Util.UserConf
import XMonad.Actions.TopicSpace hiding ( switchNthLastFocusedByScreen, workspaceHistoryByScreen )
import Workspaces.Topics
import Config.Prompts (defaultPromptTheme)

userConf :: UserConf
userConf = def
  { userConfDir = "/home/mzanic/.config"
  , userLock = "i3lock -n -i /home/mzanic/.local/share/wallpaper"
  , userEditor = myEditor
  , userModMask = mod4Mask
  , userTopics = ws
  , userTopicConfig = topicConfig
  , userPromptConfig = promptTheme
  }
  where
    ws :: [ProfileItem]
    ws = home <> notes

    promptTheme :: XPConfig
    promptTheme = defaultPromptTheme
      { height = 20
      , font = userFontStr def
      }

    myEditor :: String
    myEditor = "emacsclient -c -a'emacs'"

    topicConfig :: TopicConfig
    topicConfig = def
      { topicDirs          = tiDirs    $ map topicItem ws
      , topicActions       = tiActions $ map topicItem ws
      , defaultTopicAction = const (pure ())
      , defaultTopic       = "temp1"
      }

    home :: [ProfileItem]
    home =
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
      [ TI "dots"  "~/.config/.dotfiles"                      $ spawnEditorInTopic topicConfig 
      , TI "xmn"   "~/.local/dev/dots/xmonad-flake/xmonad"    $ spawnEditorInTopic topicConfig
      , TI "emacs" "~/.local/dev/dots/emacs-flake"            $ spawnEditorInTopic topicConfig
      , TI "xmb"   "~/.local/dev/dots/xmonad-flake/xmobar"    $ spawnEditorInTopic topicConfig
      ]
    
    notes :: [ProfileItem]
    notes = map (mkItem ["Dev", "Home", "Media"] ["tiled", "hack"])
      [ TI "notes" "~/.local/notes" $ spawnEditorInTopic topicConfig
      , inHome "temp1" $ spawnTermInTopic topicConfig
      ]
