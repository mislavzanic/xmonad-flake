module Hosts.Helpers where

import qualified Hosts.Ilija as Ilija
import qualified Hosts.Milivoj as Milivoj

import Workspaces.Topics
import XMonad.Actions.TopicSpace

topicConfig :: String -> TopicConfig
topicConfig host = case host of 
  "milivoj" -> Milivoj.topicConfig
  "ilija"   -> Ilija.topicConfig
  _         -> TopicConfig{}

topics :: String -> [ProfileItem]
topics host = case host of
  "milivoj" -> Milivoj.topics
  "ilija"   -> Ilija.topics
  _         -> []

defaultHostProfile :: String -> String
defaultHostProfile host = case host of
  "milivoj" -> Milivoj.defaultProfile
  "ilija"   -> Ilija.defaultProfile
  _         -> ""
