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
import XMonad.Util.PTL
import Config.Prompts (defaultPromptTheme)

userConf :: UserConf
userConf = def
  { userConfDir = "/home/mzanic/.config"
  , userLock = "i3lock -n -i /home/mzanic/.local/share/wallpaper"
  , userBrowser = myBrowser
  , userEditor = myEditor
  , userModMask = mod4Mask
  , userTopics = ws
  , userTopicConfig = topicConfig
  , userPromptConfig = promptTheme
  }
  where
    ws :: [ProfileTopicLayout]
    ws = home <> notes

    promptTheme :: XPConfig
    promptTheme = defaultPromptTheme
      { height = 20
      , font = userFontStr def
      }

    myEditor :: String
    myEditor = "emacsclient -c -a'emacs'"

    myBrowser :: String
    myBrowser = "firefox"

    topicConfig :: TopicConfig
    topicConfig = def
      { topicDirs          = tiDirs    $ map topicItem ws
      , topicActions       = tiActions $ map topicItem ws
      , defaultTopicAction = const (pure ())
      , defaultTopic       = "temp1"
      }

    home :: [ProfileTopicLayout]
    home =
      [ mkPTL ["Dev", "Home", "Dots"] ["tiled"] $ inHome "web" (spawn myBrowser)
      ] <> map (mkPTL ["Dev"] ["hack"])
      [ inDev  "repos"                   $ spawnEditorInTopic topicConfig
      , devDir "xmn-dev" "xmonad-dev"    $ spawnEditorInTopic topicConfig >> spawnTermInTopic topicConfig
      , devDir "nix-dev" "nix-tinkering" $ spawnEditorInTopic topicConfig >> spawnTermInTopic topicConfig
      , devDir "cp"      "compprog"      $ spawnEditorInTopic topicConfig
      ] <> map (mkPTL ["Home"] ["tiled"])
      [ inHome "rss"                      $ spawn $ myEditor <> " --eval '(elfeed)'"
      , devDir "blog" "blog/webContent"   $ spawnEditorInTopic topicConfig
      , TI     "vid"  "~/.local/torrents" $ spawnTermInTopic topicConfig
      , TI     "pdf"  "~/.local/books"    $ spawnTermInTopic topicConfig
      ] <> map (mkPTL ["Dots"] ["hack"])
      [ TI      "dots"      "~/.config/.dotfiles" $ spawnEditorInTopic topicConfig
      , dotsDir "xmn"       "xmonad-flake/xmonad" $ spawnEditorInTopic topicConfig
      , dotsDir "xmb"       "xmonad-flake/xmobar" $ spawnEditorInTopic topicConfig
      , dotsDir "emacs"     "emacs-flake"         $ spawnEditorInTopic topicConfig
      , dotsDir "nix-mods"  "nix-modules"         $ spawnEditorInTopic topicConfig
      , dotsDir "nix-utils" "nix-utils"           $ spawnEditorInTopic topicConfig
      ]

    inDev n = TI n "~/.local/dev"
    devDir n p = TI n $ "~/.local/dev/" <> p
    dotsDir n p = devDir n $ "dots/" <> p

    
    notes :: [ProfileTopicLayout]
    notes = map (mkPTL ["Dev", "Home", "Dots"] ["tiled", "hack"])
      [ TI "notes" "~/.local/notes" $ spawnEditorInTopic topicConfig
      , inHome "temp1" $ spawnTermInTopic topicConfig
      ]
