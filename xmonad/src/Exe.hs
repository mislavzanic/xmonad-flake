{-
   __   _____  ___                      _
   \ \ / /|  \/  |                     | |
    \ V / | .  . | ___  _ __   __ _  __| |
    /   \ | |\/| |/ _ \| '_ \ / _` |/ _` |
   / /^\ \| |  | | (_) | | | | (_| | (_| |
   \/   \/\_|  |_/\___/|_| |_|\__,_|\__,_|

-}

module Exe ( mzanicXmonad ) where


{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans            #-}

{-# LANGUAGE PostfixOperators    #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

import Prelude

import XMonad
import Config.Hosts


mzanicXmonad :: String -> IO ()
mzanicXmonad h = xmonad $ hostConfig h 
