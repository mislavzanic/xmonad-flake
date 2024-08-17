module Main where

import Exe ( mzanicXmonad )

import Network.HostName ( getHostName )

main :: IO ()
main = getHostName >>= mzanicXmonad
