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
  --      . usePrefixArgument "M-u"
  --      . setEwmhActivateHook activateHook
  --      . ewmhFullscreen
  --      . ewmh
  --      . docks
  --      . rescreenHook def{ afterRescreenHook = myAfterRescreenHook, randrChangeHook = myRandrChangeHook }
  --      . withUrgencyHook NoUrgencyHook
  --      . dynamicEasySBs ( pure . barSpawner h )
  --      . addProfiles profiles "Dev"
  --      $ myConfig
  -- where
  --   myConfig = desktopConfig
  --     { manageHook         = ( isFullscreen --> doFullFloat ) <+> manageDocks <+> myManageHook <+> manageHook desktopConfig
  --     , startupHook        = myStartupHook -- za postavljanje default layera
  --     , layoutHook         = lessBorders OnlyScreenFloat myLayout
  --     , handleEventHook    = handleEventHook desktopConfig <+> trayerAboveXmobarEventHook <+> myHandleEventHook
  --     , workspaces         = map (tiName . topicItem) topics
  --     , borderWidth        = myBorderWidth
  --     , terminal           = myTerminal
  --     , modMask            = myModMask
  --     , normalBorderColor  = "#333333"
  --     , focusedBorderColor = base01
  --     , logHook            = hiddenWSLogHook
  --                            <> workspaceHistoryHookExclude [scratchpadWorkspaceTag]
  --                            <> masterHistoryHook
  --                            <> updatePointer (0.5, 0.5) (0, 0)
  --     }
  --     `additionalKeysP` myKeys
