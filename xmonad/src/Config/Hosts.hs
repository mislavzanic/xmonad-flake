module Config.Hosts where

import Workspaces.Topics ( ProfileItem (topicItem) )

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
import XMonad.Hooks.WorkspaceHistory
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
import Workspaces.Profile
import XMonad.Actions.PerProfileWindows (hiddenWSLogHook)
import XMonad.Util.UserConf (UserConf(userBorderWidth, userTerminal, userModMask, userTopics, userDefaultProfile))
import XMonad (XConfig(layoutHook))
-- import XMonad.Layout.Decoration

-- baseConfig :: (ModifiedLayout l) => XConfig l
baseConfig = usePrefixArgument "M-u"
           . setEwmhActivateHook activateHook
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
      , workspaces         = map show [1..9]
      , normalBorderColor  = "#333333"
      , focusedBorderColor = base01
      , logHook            = hiddenWSLogHook
                             <> workspaceHistoryHookExclude [scratchpadWorkspaceTag]
                             <> masterHistoryHook
                             <> updatePointer (0.5, 0.5) (0, 0)
      }
      

hostConfig hostname = dynamicEasySBs ( pure . barSpawner hostname )
  . addProfiles (profiles myConf) (userDefaultProfile myConf)
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


