module Main where

import qualified Xmonad

import Network.HostName ( getHostName )
import System.Environment (getEnv)

main :: IO ()
main = do
  hostname <- getHostName
  -- configHome <- getEnv "XDG_CONFIG_HOME"
  Xmonad.main hostname  -- configHome
