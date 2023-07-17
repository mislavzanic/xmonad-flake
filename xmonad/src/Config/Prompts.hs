{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE MultiWayIf     #-}

module Config.Prompts where

import Config.Alias ( myFont )

import Theme.Xprop ( basebg, basefg, base01 )

import GHC.Exts (fromList)
import Data.Map.Strict (Map)

import XMonad
import XMonad.Actions.Search
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Input
import XMonad.Util.Run

promptTheme :: XPConfig
promptTheme = def
      { font                = myFont
      , bgColor             = basebg
      , fgColor             = basefg
      , bgHLight            = base01
      , fgHLight            = basebg
      , borderColor         = base01
      , promptBorderWidth   = 1
      , position            = CenteredAt 0.25 0.5
      , height              = 20
      , historySize         = 256
      , promptKeymap        = emacsLikeXPKeymap
      , changeModeKey       = xK_Control_R
      , historyFilter       = id
      , defaultText         = []
      , showCompletionOnTab = False
      , searchPredicate     = fuzzyMatch
      , sorter = fuzzySort
      , alwaysHighlight     = True
      , maxComplRows        = Nothing      -- set to Just 5 for 5 rows
      }

url, reddit, xmonadDocs, nixpkgs :: SearchEngine
reddit     = searchEngine  "reddit" "https://old.reddit.com/search/?q="
xmonadDocs = searchEngine  "xmonad" "https://xmonad.github.io/xmonad-docs/"
url = searchEngineF "url" \s ->
  let url = "https://"
  in if url `isPrefixOf` s then s else url <> s
nixpkgs    = searchEngine "nixpkgs" "https://search.nixos.org/packages?channel=unstable&from=0&size=30&sort=relevance&query="

searchList :: (SearchEngine -> a) -> Map (ButtonMask, KeySym) (String, a)
searchList m = fromList [ ((noModMask, xK_a), ("wolframalpha", m alpha))
                        , ((noModMask, xK_d), ("duckduckgo", m duckduckgo))
                        , ((noModMask, xK_g), ("github", m github))
                        , ((noModMask, xK_i), ("imdb", m imdb))
                        , ((noModMask, xK_n), ("nixpkgs", m nixpkgs))
                        , ((noModMask, xK_r), ("reddit", m reddit))
                        , ((noModMask, xK_s), ("url", m url))
                        , ((noModMask, xK_w), ("wikipedia", m wikipedia))
                        , ((noModMask, xK_y), ("youtube", m youtube))
                        ]
