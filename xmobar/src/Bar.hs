{-# LANGUAGE PostfixOperators    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bar where

import Xmobar (Config(commands, template), Runnable)

import qualified Hosts.Ilija as Ilija
import qualified Hosts.Milivoj as Milivoj
import Config.BarConfig

myConfig :: Int -> String -> String -> Config
myConfig n host pos = ( baseConfig n pos )
  { commands        = myCommands n host pos
  , template        = myTemplate n host pos
  }

myCommands :: Int -> String -> String -> [Runnable]
myCommands n host pos = case host of
  "ilija" -> Ilija.myCommands n pos
  "milivoj" -> Milivoj.myCommands n pos
  _ -> []

myTemplate :: Int -> String -> String -> String
myTemplate n host pos = case host of
  "ilija" -> Ilija.myTemplate n pos
  "milivoj" -> Milivoj.myTemplate n pos
  _ -> ""
