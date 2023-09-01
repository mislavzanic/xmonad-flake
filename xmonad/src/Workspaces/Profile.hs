{-# LANGUAGE LambdaCase #-}
module Workspaces.Profile where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.Profiles
import XMonad.Actions.Prefix (withPrefixArgument, PrefixArgument (Raw))
import XMonad.Actions.TopicSpace
import XMonad.Actions.ProfileWindows
import Workspaces.Topics
import XMonad.Prompt (XPConfig, XPrompt (showXPrompt), mkComplFunFromList', mkXPrompt)
import Data.Foldable
import XMonad.Util.UserConf (UserConf (userPromptConfig, userTopicConfig, userTopics))
import XMonad.Util.PTL

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

profileActionWithHidePop :: X() -> X()
profileActionWithHidePop action = withCurrentProfile hideMarkedWindows >> action >> withCurrentProfile popMarkedWindows

toggleLastProfileAndHide :: X()
toggleLastProfileAndHide = profileActionWithHidePop toggleLastProfile

switchProfilePrompt :: XPConfig -> X()
switchProfilePrompt c = do
  ps <- profileIds
  mkXPrompt (ProfilePrompt "Profile: ") c (mkComplFunFromList' c ps) switchToProfile

switchProfilePrompt' :: XPConfig -> X()
switchProfilePrompt' c = do
  ps <- profileIds
  mkXPrompt (ProfilePrompt "Profile: ") c (mkComplFunFromList' c ps) f
  where
   f :: ProfileId -> X()
   f pid = profileActionWithHidePop $ switchToProfile pid
