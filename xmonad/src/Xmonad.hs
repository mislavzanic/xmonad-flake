{-
   __   _____  ___                      _
   \ \ / /|  \/  |                     | |
    \ V / | .  . | ___  _ __   __ _  __| |
    /   \ | |\/| |/ _ \| '_ \ / _` |/ _` |
   / /^\ \| |  | | (_) | | | | (_| | (_| |
   \/   \/\_|  |_/\___/|_| |_|\__,_|\__,_|

-}

module Xmonad ( main ) where


{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans            #-}

{-# LANGUAGE PostfixOperators    #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

-- import Workspaces.Topics ( topics, ProfileItem (topicItem) )

import Prelude

import XMonad
import Config.Hosts


main :: String -> IO ()
main h = xmonad $ hostConfig h 
-- main :: String -> String -> IO ()
-- main h configDir = xmonad $ hostConfig h configDir
