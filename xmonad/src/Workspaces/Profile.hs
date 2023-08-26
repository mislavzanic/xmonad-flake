{-# LANGUAGE LambdaCase #-}
module Workspaces.Profile where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.Profiles
import XMonad.Actions.Prefix (withPrefixArgument, PrefixArgument (Raw))
import XMonad.Actions.TopicSpace
import Workspaces.Topics
import XMonad.Prompt (XPConfig, XPrompt (showXPrompt), mkComplFunFromList', mkXPrompt)
import Data.Foldable
import XMonad.Actions.PerProfileWindows (hideBeforeSwitch, showAfterSwitch)
import XMonad.Util.UserConf (UserConf (userPromptConfig, userTopicConfig, userTopics))
import XMonad.Util.PTL

import XMonad.Layout.PerProfileWindows

profileKeys :: UserConf -> [(String, X())]
profileKeys = topicKeys 

profiles :: UserConf -> [Profile]
profiles conf = getProfiles $ userTopics conf

topicKeys :: UserConf -> [(String, X())]
topicKeys conf =
  [ ("M-a", withPrefixArgument $
              \case Raw _ -> spawnTermInTopic tc >> spawnEditorInTopic tc
                    _     -> currentTopicAction tc
    )
  , ("M-g", switchProfileTopicPrompt tc promptTheme)
  , ("M-S-g", shiftProfileTopicPrompt promptTheme)
  , ("M1-<Tab>", toggleTopic tc)

  , ("M1-u", spawnEditorInTopic tc)
  , ("M-<Return>", spawnTermInTopic tc)
  ] ++
  [ ("M-" ++ m ++ k, bindOn $ map (\x -> (head $ pId x, f $ (tiName . topicItem) x)) i)
  | (i, k) <- bindProfileWSKeys $ userTopics conf
  , (f, m) <- [(mby $ goto tc, ""), (mby $ windows . W.shift, "S-")]
  ]
  where
    mby f tn = if tn == "" then return () else f tn
    tc = userTopicConfig conf

    promptTheme = userPromptConfig conf

toggleLastProfile :: X()
toggleLastProfile = previousProfile >>= (`forM_` switchToProfile)

toggleLastProfileWithHiding :: X()
toggleLastProfileWithHiding = do
  cp <- currentProfile
  pp <- previousProfile
  forM_ pp hideBeforeSwitch >> forM_ pp switchToProfile
  showAfterSwitch cp

switchProfilePrompt :: XPConfig -> X()
switchProfilePrompt c = do
  ps <- profileIds
  mkXPrompt (ProfilePrompt "Profile: ") c (mkComplFunFromList' c ps) switchToProfile

switchProfilePrompt' :: XPConfig -> X()
switchProfilePrompt' c = do
  ps <- profileIds
  mkXPrompt (ProfilePrompt "Profile: ") c (mkComplFunFromList' c ps) switchAndHandleMarked
  where
   switchAndHandleMarked pid = hideMarkedWindows >> switchToProfile pid >> popMarkedWindows

switchProfileWithHidingPrompt :: XPConfig -> X()
switchProfileWithHidingPrompt c = do
  ps <- profileIds
  mkXPrompt (ProfilePrompt "Profile: ") c (mkComplFunFromList' c ps) switchToProfileWithHiding
  where
    switchToProfileWithHiding p = do
      cp <- currentProfile
      hideBeforeSwitch cp >> switchToProfile p >> showAfterSwitch cp
