{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE DerivingVia #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.Profiles
-- Description :  Sort your workspaces into profiles (groups).
-- Copyright   :  (c) Mislav Zanic 2023
-- License     :  BSD3-style
--
-- Maintainer  :  <mislavzanic3@gmail.com>
-- Stability   :  experimental
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module XMonad.Actions.Profiles
  ( ProfileId
  , Profile(..)
  , addProfiles
  , addProfilesWithHistory
  , addProfilesWithHistoryExclude
  , addCurrentWSToProfilePrompt
  , currentProfile
  , previousProfile
  , profileHistory
  , excludeWSPP
  , profileIds
  , switchToProfile
  , allProfileWindows
  , profileMap
  , currentProfileWorkspaces
  , profileWorkspaces
  , wsFilter
  , switchWSOnScreens
  , bindOn
  , profileLogger
  , setProfile
  , ProfilePrompt(..)
  )where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Control.DeepSeq

import XMonad
import XMonad.Prelude
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS

import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Loggers (Logger)
import XMonad.Prompt.Window (XWindowMap)
import XMonad.Actions.WindowBringer (WindowBringerConfig(..))
import XMonad.Actions.OnScreen (greedyViewOnScreen)
import XMonad.Hooks.Rescreen (addAfterRescreenHook)
import XMonad.Hooks.DynamicLog (PP(ppRename))
import XMonad.Prompt 

type ProfileId  = String
type ProfileMap = Map ProfileId Profile

data Profile = Profile
  { profileId :: !ProfileId
  , profileWS :: ![WorkspaceId]
  }

data ProfileState = ProfileState
  { profiles :: !ProfileMap
  , current  :: !(Maybe Profile)
  , previous :: !(Maybe ProfileId)
  }

instance ExtensionClass ProfileState where
  initialValue = ProfileState Map.empty Nothing Nothing

newtype ProfileHistory = ProfileHistory
  { history :: Map ProfileId [(ScreenId, WorkspaceId)]
  }
  deriving (Read, Show)
  deriving NFData via Map ProfileId [(Int, WorkspaceId)]

instance ExtensionClass ProfileHistory where
  extensionType = PersistentExtension
  initialValue = ProfileHistory Map.empty

newtype ProfilePrompt = ProfilePrompt String

instance XPrompt ProfilePrompt where
  showXPrompt (ProfilePrompt x) = x

currentProfile :: X ProfileId
currentProfile = profileId . fromMaybe (Profile "default" []) . current <$> XS.get

previousProfile :: X (Maybe ProfileId)
previousProfile = XS.gets previous

profileHistory :: X (Map ProfileId [(ScreenId, WorkspaceId)])
profileHistory = XS.gets history

-- | Hook profiles into XMonad
addProfiles :: [Profile] -> ProfileId -> XConfig a -> XConfig a
addProfiles pfs dp conf = addAfterRescreenHook (currentProfile >>= switchWSOnScreens) $ conf
  { startupHook = profilesStartupHook pfs dp <> startupHook conf
  }

addProfilesWithHistory :: [Profile] -> ProfileId -> XConfig a -> XConfig a
addProfilesWithHistory pfs dp conf = addAfterRescreenHook (currentProfile >>= switchWSOnScreens) $ conf
  { startupHook = profilesStartupHook pfs dp <> startupHook conf
  , logHook = profileHistoryHook <> logHook conf
  }

addProfilesWithHistoryExclude :: [Profile] -> ProfileId -> [WorkspaceId] -> XConfig a -> XConfig a
addProfilesWithHistoryExclude pfs dp ws conf = addAfterRescreenHook (currentProfile >>= switchWSOnScreens) $ conf
  { startupHook = profilesStartupHook pfs dp <> startupHook conf
  , logHook = profileHistoryHookExclude ws <> logHook conf
  }

profileHistoryHook :: X()
profileHistoryHook = profileHistoryHookExclude []

profileHistoryHookExclude :: [WorkspaceId] -> X()
profileHistoryHookExclude ews = do
  cur <- gets $ W.current . windowset
  vis <- gets $ W.visible . windowset
  pws <- currentProfileWorkspaces
  p <- currentProfile

  updateHist p $ workspaceScreenPairs $ filter (filterWS pws) $ cur:vis
  where
    workspaceScreenPairs wins = zip (W.screen <$> wins) (W.tag . W.workspace <$> wins)
    filterWS pws = (\wid -> (wid `elem` pws) && (wid `notElem` ews)) . W.tag . W.workspace

updateHist :: ProfileId -> [(ScreenId, WorkspaceId)] -> X()
updateHist pid xs = XS.modify' update
  where
    update hs = force $ hs { history = doUpdate $ history hs }
    doUpdate hist = foldl (\acc (sid, wid) -> Map.alter (f sid wid) pid acc) hist xs
    f sid wid val = case val of
      Nothing -> pure [(sid, wid)]
      Just hs -> pure $ let new = (sid, wid) in new: delete new hs

profilesStartupHook :: [Profile] -> ProfileId -> X ()
profilesStartupHook ps pid = XS.modify go >> switchWSOnScreens pid
  where
    go :: ProfileState -> ProfileState
    go s = s {profiles = update $ profiles s, current = setCurrentProfile $ Map.fromList $ map entry ps}

    update :: ProfileMap -> ProfileMap
    update = Map.union (Map.fromList $ map entry ps)

    entry :: Profile -> (ProfileId, Profile)
    entry p = (profileId p, p)

    setCurrentProfile :: ProfileMap -> Maybe Profile
    setCurrentProfile s = case Map.lookup pid s of
      Nothing -> Just $ Profile pid []
      Just pn -> Just pn

setPrevious :: ProfileId -> X()
setPrevious name = XS.modify update
  where
    update ps = ps { previous = doUpdate ps }
    doUpdate ps = case Map.lookup name $ profiles ps of
      Nothing -> previous ps
      Just p -> Just $ profileId p

setProfile :: ProfileId -> X ()
setProfile p = currentProfile >>= setPrevious >> setProfile' p

setProfile' :: ProfileId -> X ()
setProfile' name = XS.modify update
  where
    update ps = ps { current = doUpdate ps }
    doUpdate ps = case Map.lookup name $ profiles ps of
      Nothing -> current ps
      Just p -> Just p

profileIds :: X [ProfileId]
profileIds = Map.keys <$> XS.gets profiles

switchToProfile :: ProfileId -> X()
switchToProfile pid = setProfile pid >> switchWSOnScreens pid

currentProfileWorkspaces :: X [WorkspaceId]
currentProfileWorkspaces = XS.gets current <&> profileWS . fromMaybe (Profile "default" [])

profileWorkspaces :: ProfileId -> X [WorkspaceId]
profileWorkspaces pid = profileMap >>= findPWs
  where
    findPWs pm = return . profileWS . fromMaybe (Profile "default" []) $ Map.lookup pid pm

addCurrentWSToProfilePrompt :: XPConfig -> X()
addCurrentWSToProfilePrompt c = do
  ps <- profileIds
  mkXPrompt (ProfilePrompt "Add to profile:") c (mkComplFunFromList' c ps) addCurrentWSToProfile

addCurrentWSToProfile :: ProfileId -> X()
addCurrentWSToProfile pid = do
  cur <- gets $ W.tag . W.workspace . W.current . windowset
  addWSToProfile cur pid

addWSToProfile :: WorkspaceId -> ProfileId -> X()
addWSToProfile wid pid = XS.modify' go
  where
   go :: ProfileState -> ProfileState
   go ps = ps {profiles = update $ profiles ps}

   update :: ProfileMap -> ProfileMap
   update mp = case Map.lookup pid mp of
     Nothing -> mp
     Just p  -> if wid `elem` profileWS p then mp else Map.adjust f pid mp

   f :: Profile -> Profile
   f p = Profile pid (wid : profileWS p) 

profileMap :: X ProfileMap
profileMap = XS.gets profiles

excludeWSPP :: PP -> X PP
excludeWSPP pp = modifyPP <$> currentProfileWorkspaces
  where
    modifyPP pws = pp { ppRename = ppRename pp . printTag pws }
    printTag pws tag = if tag `elem` pws then tag else ""

-- | For cycling through workspaces associated with the current
wsFilter :: WSType
wsFilter = WSIs $ currentProfileWorkspaces >>= (\ws -> return $ (`elem` ws) . W.tag)

switchWSOnScreens :: ProfileId -> X()
switchWSOnScreens pid = do
  hist <- profileHistory
  vis <- gets $ W.visible . windowset
  cur <- gets $ W.current . windowset
  pws <- profileMap <&> (profileWS . fromMaybe (Profile pid []) . Map.lookup pid)
  case Map.lookup pid hist of
    Nothing -> switchScreens $ zip (W.screen <$> cur:vis) pws
    Just xs -> compareAndSwitch (uniq . reverse $ xs) (cur:vis) pws
  where
    uniq = Map.toList . Map.fromList
    viewWS fview sid wid = windows $ fview sid wid
    switchScreens = mapM_ (uncurry $ viewWS greedyViewOnScreen)
    compareAndSwitch wss wins pws | length wss > length wins  = switchScreens $ filter ((`elem` (W.screen <$> wins)) . fst) wss
                                  | length wss == length wins = switchScreens wss
                                  | otherwise                 = switchScreens $ wss <> zip (filter (`notElem` map fst wss) $ W.screen <$> wins) (filter (`notElem` map snd wss) pws)

chooseAction :: (String -> X ()) -> X ()
chooseAction f = XS.gets current <&> (profileId . fromMaybe (Profile "default" [])) >>= f

-- | Keybindings per Profile
bindOn :: [(String, X ())] -> X ()
bindOn bindings = chooseAction chooser
  where
    chooser profile = case lookup profile bindings of
        Just action -> action
        Nothing -> case lookup "" bindings of
            Just action -> action
            Nothing -> return ()

-- | Loggs currentProfile and all profiles with hidden workspaces
profileLogger :: (String -> String) -> (String -> String) -> Logger
profileLogger formatFocused formatUnfocused = do
  hws <- gets $ W.hidden . windowset
  p <- currentProfile
  hm <- map fst
      . filter (\(p', xs) -> any ((`elem` htags hws) . snd) xs || p' == p)
      . Map.toList <$> profileHistory
  return $ Just $ foldl (\a b -> a ++ " " ++ b) "" $ format p <$> hm
  where
    format p a = if a == p then formatFocused a else formatUnfocused a
    htags wins = W.tag <$> filter (isJust . W.stack) wins

allProfileWindows :: XWindowMap
allProfileWindows = allProfileWindows' def

allProfileWindows' :: WindowBringerConfig -> XWindowMap
allProfileWindows' WindowBringerConfig{ windowTitler = titler, windowFilter = include } = do
  pws <- currentProfileWorkspaces
  windowSet <- gets windowset
  Map.fromList . concat <$> mapM keyValuePairs (filter ((`elem` pws) . W.tag) $ W.workspaces windowSet)
   where keyValuePairs ws = let wins = W.integrate' (W.stack ws)
                           in mapM (keyValuePair ws) =<< filterM include wins
         keyValuePair ws w = (, w) <$> titler ws w
