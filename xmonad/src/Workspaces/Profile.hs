{-# LANGUAGE LambdaCase #-}
module Workspaces.Profile where


import qualified Data.Set as Set

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.Profiles
import XMonad.Actions.Prefix (withPrefixArgument, PrefixArgument (Raw))
import Config.Prompts
import XMonad.Actions.TopicSpace
import Workspaces.Topics
import XMonad.Prompt (XPConfig, XPrompt (showXPrompt), mkComplFunFromList', mkXPrompt)
import Data.Foldable
import Data.List (groupBy, sortBy)
import XMonad.Actions.PerProfileWindows (hideBeforeSwitch, showAfterSwitch)
import XMonad.Util.UserConf (UserConf (userPromptConfig, userTopicConfig, userTopics))

profileKeys :: UserConf -> [(String, X())]
profileKeys = topicKeys 

topicProfiles :: UserConf -> [Profile]
topicProfiles conf = (\p -> Profile p $ getTopics p $ userTopics conf) <$> ps
  where
    ps = Set.toList . Set.fromList $ foldl (\acc pi' -> acc <> pId pi') [] $ userTopics conf
    getTopics pid ts = map (tiName . topicItem) $ filter ((pid `elem`) . pId) ts

profiles :: UserConf -> [Profile]
profiles = topicProfiles 

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
  | (i, k) <- groups
  , (f, m) <- [(mby $ goto tc, ""), (mby $ windows . W.shift, "S-")]
  ]
  where
    mby f tn = if tn == "" then return () else f tn
    tc = userTopicConfig conf

    promptTheme = userPromptConfig conf

    groups :: [([ProfileItem], String)]
    groups = map (\p -> (map fst p, snd . head $ p))
           . sortGroupBy snd
           . concatMap (\x -> zip (x <> repeat (mkItem (pId $ head x) [] $ TI "" "" $ return ()))  (map show [1..9 :: Int]))
           . sortGroupBy (head . pId)
           . concatMap (\p -> map (\pid -> mkItem [pid] (layouts p) $ topicItem p) $ pId p) $ userTopics conf

    sortGroupBy f = groupBy (\x y -> f x == f y) . sortBy (\x y -> compare (f x) (f y))

toggleLastProfile :: X()
toggleLastProfile = previousProfile >>= (`forM_` switchToProfile)

toggleLastProfileWithHiding :: X()
toggleLastProfileWithHiding = do
  cp <- currentProfile
  pp <- previousProfile
  forM_ pp hideBeforeSwitch >> forM_ pp switchToProfile
  showAfterSwitch cp

newtype ProfilePrompt = ProfilePrompt String

instance XPrompt ProfilePrompt where
  showXPrompt (ProfilePrompt x) = x


switchProfilePrompt :: XPConfig -> X()
switchProfilePrompt c = do
  ps <- profileIds
  mkXPrompt (ProfilePrompt "Profile: ") c (mkComplFunFromList' c ps) switchToProfile

switchProfileWithHidingPrompt :: XPConfig -> X()
switchProfileWithHidingPrompt c = do
  ps <- profileIds
  mkXPrompt (ProfilePrompt "Profile: ") c (mkComplFunFromList' c ps) switchToProfileWithHiding
  where
    switchToProfileWithHiding p = do
      cp <- currentProfile
      hideBeforeSwitch cp >> switchToProfile p >> showAfterSwitch cp
