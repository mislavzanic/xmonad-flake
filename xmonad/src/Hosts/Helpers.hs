module Hosts.Helpers where

import qualified Hosts.Ilija as Ilija
import qualified Hosts.Mzanic as Mzanic

import Workspaces.Topics
import XMonad.Actions.TopicSpace

topicConfig :: String -> TopicConfig
topicConfig host = case host of 
  "mzanic"  -> Mzanic.topicConfig
  "ilija"   -> Ilija.topicConfig
  _         -> TopicConfig{}

topics :: String -> [ProfileItem]
topics host = case host of
  "mzanic" -> Mzanic.topics
  "ilija"   -> Ilija.topics
  _         -> []

defaultHostProfile :: String -> String
defaultHostProfile host = case host of
  "mzanic" -> Mzanic.defaultProfile
  "ilija"   -> Ilija.defaultProfile
  _         -> ""
