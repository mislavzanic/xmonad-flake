module Hosts.Helpers where

import qualified Hosts.Ilija as Ilija
import qualified Hosts.Default as Default

import Workspaces.Topics
import XMonad.Actions.TopicSpace

topicConfig :: String -> TopicConfig
topicConfig host = case host of 
  "ilija"   -> Ilija.topicConfig
  _         -> Default.topicConfig

topics :: String -> [ProfileItem]
topics host = case host of
  "ilija"   -> Ilija.topics
  _         -> Default.topics

defaultHostProfile :: String -> String
defaultHostProfile host = case host of
  "ilija"   -> Ilija.defaultProfile
  _         -> Default.defaultProfile
