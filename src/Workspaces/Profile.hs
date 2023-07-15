{-# LANGUAGE LambdaCase #-}
module Workspaces.Profile where


import qualified Data.Set as Set

import Hosts.Helpers

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

profileKeys :: String -> [(String, X())]
profileKeys = topicKeys 

topicProfiles :: String -> [Profile]
topicProfiles host = (\p -> Profile p $ getTopics p $ topics host) <$> ps
  where
    ps = Set.toList . Set.fromList $ foldl (\acc pi' -> acc <> pId pi') [] $ topics host
    getTopics pid ts = map (tiName . topicItem) $ filter ((pid `elem`) . pId) ts

profiles :: String -> [Profile]
profiles = topicProfiles 

topicKeys :: String -> [(String, X())]
topicKeys host =
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
    tc = topicConfig host

    groups :: [([ProfileItem], String)]
    groups = map (\p -> (map fst p, snd . head $ p))
           . sortGroupBy snd
           . concatMap (\x -> zip (x <> repeat (mkItem (pId $ head x) [] $ TI "" "" $ return ()))  (map show [1..9 :: Int]))
           . sortGroupBy (head . pId)
           . concatMap (\p -> map (\pid -> mkItem [pid] (layouts p) $ topicItem p) $ pId p) $ topics host

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
