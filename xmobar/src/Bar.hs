{-# LANGUAGE PostfixOperators    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bar where

import Xmobar (Config(commands, template), Runnable)

import qualified Hosts.Ilija as Ilija
import qualified Hosts.Mzanic as Mzanic
import Config.BarConfig

myConfig :: Int -> String -> String -> Config
myConfig n host pos = (hostConfig host) $ ( baseConfig n pos )
  { commands        = myCommands n host pos
  , template        = myTemplate n host pos
  }

myCommands :: Int -> String -> String -> [Runnable]
myCommands n host pos = case host of
  "ilija" -> Ilija.myCommands n pos
  _ -> Mzanic.myCommands n pos

hostConfig :: String -> Config -> Config
hostConfig host = case host of
  "ilija" -> Ilija.myConfig
  _       -> Mzanic.myConfig

myTemplate :: Int -> String -> String -> String
myTemplate n host pos = case host of
  "ilija" -> Ilija.myTemplate n pos
  _ -> Mzanic.myTemplate n pos
