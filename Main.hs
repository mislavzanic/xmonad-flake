module Main where

import qualified Xmonad

import Network.HostName ( getHostName )

main :: IO ()
main = do
  hostname <- getHostName
  Xmonad.main hostname
