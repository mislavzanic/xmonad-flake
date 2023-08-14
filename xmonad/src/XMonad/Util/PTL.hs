{-# LANGUAGE DerivingVia #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.PTL
-- Description :  Group profiles, topics and layouts
-- Copyright   :  (c) Mislav Zanic 2023
-- License     :  BSD3-style
--
-- Maintainer  :  <mislavzanic3@gmail.com>
-- Stability   :  experimental
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module XMonad.Util.PTL
  ( ProfileTopicLayout(..)
  , mkPTL
  , getProfiles
  )where

import qualified Data.Set as Set

import XMonad.Actions.Profiles
import XMonad.Actions.TopicSpace


data ProfileTopicLayout = PTL
  { topicItem :: !TopicItem
  , pId       :: ![ProfileId]
  , layouts   :: [String]
  }

mkPTL :: [ProfileId] -> [String] -> TopicItem -> ProfileTopicLayout
mkPTL pids ls ti = PTL ti pids ls

getProfiles :: [ProfileTopicLayout] -> [Profile]
getProfiles ptls = (\p -> Profile p $ getTopics p ptls) <$> ps
  where
    ps = Set.toList . Set.fromList $ foldl (\acc pi' -> acc <> pId pi') [] ptls
    getTopics pid ts = map (tiName . topicItem) $ filter ((pid `elem`) . pId) ts
