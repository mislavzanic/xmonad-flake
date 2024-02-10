module Config.Hosts where

import Config.Bar ( barSpawner )
import Config.Keys ( myKeys )

import Hosts.Default as Default
import Hosts.Mzanic as Mzanic

import Hooks.ManageHook ( activateHook, myManageHook )
import Hooks.LayoutHook ( myLayout )
import Hooks.HandleEventHook ( myHandleEventHook )
import Hooks.StartupHook ( myStartupHook )
import Hooks.RescreenHook
import Theme.Xprop

import XMonad

import Prelude

-- config
import XMonad.Config.Desktop ( desktopConfig )

-- actions
import XMonad.Actions.SwapPromote (masterHistoryHook)
import XMonad.Actions.TopicSpace (TopicItem (tiName))
import XMonad.Actions.UpdatePointer (updatePointer)

-- util
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad (scratchpadWorkspaceTag)

-- layout
import XMonad.Layout.NoBorders ( lessBorders, Ambiguity(OnlyScreenFloat) )

-- hooks
import XMonad.Hooks.Rescreen
import XMonad.Hooks.ManageDocks ( manageDocks, docks )
import XMonad.Hooks.EwmhDesktops ( setEwmhActivateHook, ewmhFullscreen, ewmh )
import XMonad.Hooks.ManageHelpers ( doFullFloat, isFullscreen )
import XMonad.Hooks.UrgencyHook ( withUrgencyHook, NoUrgencyHook(NoUrgencyHook) )
import XMonad.Hooks.StatusBar ( dynamicEasySBs )
import XMonad.Actions.Prefix (usePrefixArgument)
import XMonad.Util.Hacks (trayerAboveXmobarEventHook)
import XMonad.Actions.Profiles
import XMonad.Actions.ProfileWindows
import qualified Workspaces.Profile as WP
import XMonad.Util.UserConf (UserConf(userBorderWidth, userTerminal, userModMask, userTopics, userDefaultProfile))
import XMonad.Util.PTL


baseConfig = setEwmhActivateHook activateHook
           . ewmhFullscreen
           . ewmh
           . docks
           . rescreenHook def{ afterRescreenHook = myAfterRescreenHook, randrChangeHook = myRandrChangeHook }
           . withUrgencyHook NoUrgencyHook
           $ myConfig
  where 
    myConfig = desktopConfig
      { manageHook         = ( isFullscreen --> doFullFloat ) <+> manageDocks <+> myManageHook <+> manageHook desktopConfig
      , startupHook        = myStartupHook
      , handleEventHook    = handleEventHook desktopConfig <+> trayerAboveXmobarEventHook <+> myHandleEventHook
      , workspaces         = map show [1..9 :: Int]
      , normalBorderColor  = "#333333"
      , focusedBorderColor = base01
      , logHook            = masterHistoryHook <> updatePointer (0.5, 0.5) (0, 0)
      }
      

hostConfig hostname = usePrefixArgument "M-f"
  . dynamicEasySBs ( pure . barSpawner hostname )
  . addProfileWindows
  . addProfilesWithHistory def
                           { workspaceExcludes = [scratchpadWorkspaceTag]
                           , profiles          = WP.profiles myConf
                           , startingProfile   = userDefaultProfile myConf
                           }
  $ baseConfig
  { workspaces  = map (tiName . topicItem) $ userTopics myConf
  , layoutHook  = lessBorders OnlyScreenFloat $ myLayout myConf
  , borderWidth = userBorderWidth myConf
  , terminal    = userTerminal myConf
  , modMask     = userModMask myConf
  } `additionalKeysP` myKeys myConf
  where
   myConf = case hostname of
     "mzanic" -> Mzanic.userConf
     _        -> Default.userConf
