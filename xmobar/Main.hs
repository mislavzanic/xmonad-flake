{-# LANGUAGE PostfixOperators    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}

{-
   __   __                _
   \ \ / /               | |
    \ V / _ __ ___   ___ | |__   __ _ _ __
    /   \| '_ ` _ \ / _ \| '_ \ / _` | '__|
   / /^\ \ | | | | | (_) | |_) | (_| | |
   \/   \/_| |_| |_|\___/|_.__/ \__,_|_|
-}

import Xmobar ( xmobar )

import Bar ( myConfig )

import System.Environment (getArgs)

defaultHost :: String
defaultHost = "milivoj"

main :: IO ()
main = getArgs >>= \case
  ["-x", n, "-h", host, "-p", pos]    -> xmobar $ myConfig (read n) host pos
  ["-x", n, "-h", host, "-p", pos, _] -> xmobar $ myConfig (read n) host pos
  _                                   -> xmobar $ myConfig 0 defaultHost "top"
