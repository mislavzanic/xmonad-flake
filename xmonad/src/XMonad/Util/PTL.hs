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
  , bindProfileWSKeys
  )where

import qualified Data.Set as Set
import Data.List (groupBy, sortBy)

import XMonad.Actions.Profiles
import XMonad.Actions.TopicSpace

data ProfileTopicLayout = PTL
  { topicItem :: !TopicItem
  , pId       :: ![ProfileId]
  , layouts   :: [String]
  }

mkPTL :: [ProfileId] -> [String] -> TopicItem -> ProfileTopicLayout
mkPTL pids ls ti = PTL ti pids ls

bindProfileWSKeys :: [ProfileTopicLayout] -> [([ProfileTopicLayout], String)]
bindProfileWSKeys = map (\p -> (map fst p, snd . head $ p))
                  . sortGroupBy snd
                  . concatMap (\x -> zip (x <> repeat (mkPTL (pId $ head x) [] $ TI "" "" $ return ()))  (map show [1..9 :: Int]))
                  . sortGroupBy (head . pId)
                  . concatMap (\p -> map (\pid -> mkPTL [pid] (layouts p) $ topicItem p) $ pId p)
  where
    sortGroupBy f = groupBy (\x y -> f x == f y) . sortBy (\x y -> compare (f x) (f y))

getProfiles :: [ProfileTopicLayout] -> [Profile]
getProfiles ptls = (\p -> Profile p (getTopics p ptls)) <$> ps
  where
    ps = Set.toList . Set.fromList $ foldl (\acc pi' -> acc <> pId pi') [] ptls
    getTopics pid ts = map (tiName . topicItem) $ filter ((pid `elem`) . pId) ts
