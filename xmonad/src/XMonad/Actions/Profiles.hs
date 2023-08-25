{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DerivingVia   #-}


--------------------------------------------------------------------------------
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
--------------------------------------------------------------------------------

module XMonad.Actions.Profiles
  ( ProfileId
  , Profile(..)
  , ProfilePrompt(..)
  , ProfileConfig(..)
  , addProfiles
  , addProfilesWithHistory
  , addCurrentWSToProfilePrompt
  , addWSToProfilePrompt
  , removeWSFromProfilePrompt
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
  )where

import Data.Map.Strict (Map)
import Data.List
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

--------------------------------------------------------------------------------
type ProfileId  = String
type ProfileMap = Map ProfileId Profile

--------------------------------------------------------------------------------
data Profile = Profile
  { profileId :: !ProfileId
  , profileWS :: ![WorkspaceId]
  }

--------------------------------------------------------------------------------
data ProfileState = ProfileState
  { profilesMap :: !ProfileMap
  , current     :: !(Maybe Profile)
  , previous    :: !(Maybe ProfileId)
  }

--------------------------------------------------------------------------------
data ProfileConfig = ProfileConfig
  { workspaceExcludes :: ![WorkspaceId]
  , profiles          :: ![Profile]
  , startingProfile   :: !ProfileId
  }

--------------------------------------------------------------------------------
instance Default ProfileConfig where
  def            = ProfileConfig { workspaceExcludes = []
                                 , profiles          = []
                                 , startingProfile   = "default"
                                 }

--------------------------------------------------------------------------------
instance ExtensionClass ProfileState where
  initialValue = ProfileState Map.empty Nothing Nothing

--------------------------------------------------------------------------------
newtype ProfileHistory = ProfileHistory
  { history :: Map ProfileId [(ScreenId, WorkspaceId)]
  }
  deriving (Read, Show)
  deriving NFData via Map ProfileId [(Int, WorkspaceId)]

--------------------------------------------------------------------------------
instance ExtensionClass ProfileHistory where
  extensionType = PersistentExtension
  initialValue = ProfileHistory Map.empty

--------------------------------------------------------------------------------
newtype ProfilePrompt = ProfilePrompt String

--------------------------------------------------------------------------------
instance XPrompt ProfilePrompt where
  showXPrompt (ProfilePrompt x) = x

--------------------------------------------------------------------------------
currentProfile :: X ProfileId
currentProfile = profileId . fromMaybe (Profile "default" []) . current <$> XS.get

previousProfile :: X (Maybe ProfileId)
previousProfile = XS.gets previous

profileHistory :: X (Map ProfileId [(ScreenId, WorkspaceId)])
profileHistory = XS.gets history

profileMap :: X ProfileMap
profileMap = XS.gets profilesMap

profileIds :: X [ProfileId]
profileIds = Map.keys <$> XS.gets profilesMap

currentProfileWorkspaces :: X [WorkspaceId]
currentProfileWorkspaces = XS.gets current <&> profileWS . fromMaybe (Profile "default" [])

--------------------------------------------------------------------------------
-- | Hook profiles into XMonad. This function adds a startup hook that
-- sets up ProfileState. Also adds an afterRescreenHook for viewing correct
-- workspaces when adding new screens.
addProfiles :: ProfileConfig -> XConfig a -> XConfig a
addProfiles profConf conf = addAfterRescreenHook hook $ conf
  { startupHook = profileStartupHook' <> startupHook conf
  }
 where
   profileStartupHook' :: X()
   profileStartupHook' = profilesStartupHook (profiles profConf) (startingProfile profConf)
   hook = currentProfile >>= switchWSOnScreens

--------------------------------------------------------------------------------
-- | Hooks profiles into XMonad and enables Profile history logging.
addProfilesWithHistory :: ProfileConfig -> XConfig a -> XConfig a
addProfilesWithHistory profConf conf = conf'
  { logHook = profileHistoryHookExclude (workspaceExcludes profConf) <> logHook conf
  }
  where
   conf' = addProfiles profConf conf

--------------------------------------------------------------------------------
profileHistoryHookExclude :: [WorkspaceId] -> X()
profileHistoryHookExclude ews = do
  cur <- gets $ W.current . windowset
  vis <- gets $ W.visible . windowset
  pws <- currentProfileWorkspaces
  p <- currentProfile

  updateHist p $ workspaceScreenPairs $ filterWS pws $ cur:vis
  where
    workspaceScreenPairs wins = zip (W.screen <$> wins) (W.tag . W.workspace <$> wins)
    filterWS pws = filter ((\wid -> (wid `elem` pws) && (wid `notElem` ews)) . W.tag . W.workspace)

--------------------------------------------------------------------------------
updateHist :: ProfileId -> [(ScreenId, WorkspaceId)] -> X()
updateHist pid xs = profileWorkspaces pid >>= XS.modify' . update
  where
    update pws hs = force $ hs { history = doUpdate pws $ history hs }

    doUpdate pws hist = foldl (\acc (sid, wid) -> Map.alter (f pws sid wid) pid acc) hist xs

    f pws sid wid val = case val of
      Nothing -> pure [(sid, wid)]
      Just hs -> pure $ let new = (sid, wid) in new:filterWS pws new hs

    filterWS :: [WorkspaceId] -> (ScreenId, WorkspaceId) -> [(ScreenId, WorkspaceId)] -> [(ScreenId, WorkspaceId)]
    filterWS pws new = filter (\x -> snd x `elem` pws && x /= new)

--------------------------------------------------------------------------------
-- | Adds profiles to ProfileState and sets current profile.
profilesStartupHook :: [Profile] -> ProfileId -> X ()
profilesStartupHook ps pid = XS.modify go >> switchWSOnScreens pid
  where
    go :: ProfileState -> ProfileState
    go s = s {profilesMap = update $ profilesMap s, current = setCurrentProfile $ Map.fromList $ map entry ps}

    update :: ProfileMap -> ProfileMap
    update = Map.union (Map.fromList $ map entry ps)

    entry :: Profile -> (ProfileId, Profile)
    entry p = (profileId p, p)

    setCurrentProfile :: ProfileMap -> Maybe Profile
    setCurrentProfile s = case Map.lookup pid s of
      Nothing -> Just $ Profile pid []
      Just pn -> Just pn

--------------------------------------------------------------------------------
-- | Updates the value of the previous variable.
setPrevious :: ProfileId -> X()
setPrevious name = XS.modify update
  where
    update ps = ps { previous = doUpdate ps }
    doUpdate ps = case Map.lookup name $ profilesMap ps of
      Nothing -> previous ps
      Just p -> Just $ profileId p

--------------------------------------------------------------------------------
-- | Sets the previous variable to the current. Updates the current variable.
setProfile :: ProfileId -> X ()
setProfile p = currentProfile >>= setPrevious >> setProfile' p

-- | Updates the current variable.
setProfile' :: ProfileId -> X ()
setProfile' name = XS.modify update
  where
    update ps = ps { current = doUpdate ps }
    doUpdate ps = case Map.lookup name $ profilesMap ps of
      Nothing -> current ps
      Just p -> Just p

--------------------------------------------------------------------------------
-- | Sets the current variable to the value of pid and 
-- switches to the correct workspaces.
switchToProfile :: ProfileId -> X()
switchToProfile pid = setProfile pid >> switchWSOnScreens pid

--------------------------------------------------------------------------------
-- | Returns the workspace ids associated with a profile id.
profileWorkspaces :: ProfileId -> X [WorkspaceId]
profileWorkspaces pid = profileMap >>= findPWs
  where
    findPWs pm = return . profileWS . fromMaybe (Profile "default" []) $ Map.lookup pid pm

--------------------------------------------------------------------------------
addWSToProfilePrompt :: XPConfig -> X()
addWSToProfilePrompt c = do
  ps <- profileIds
  mkXPrompt (ProfilePrompt "Add ws to profile:") c (mkComplFunFromList' c ps) f
  where
   f :: String -> X()
   f p = do
     vis <- gets $ fmap (W.tag . W.workspace) . W.visible . windowset
     cur <- gets $ W.tag . W.workspace . W.current . windowset
     hid <- gets $ fmap W.tag . W.hidden . windowset
     let
       arr = cur:(vis <> hid)
       in mkXPrompt (ProfilePrompt "Ws to add to profile:") c (mkComplFunFromList' c arr) (`addWSToProfile` p)
     
--------------------------------------------------------------------------------
addCurrentWSToProfilePrompt :: XPConfig -> X()
addCurrentWSToProfilePrompt c = do
  ps <- profileIds
  mkXPrompt (ProfilePrompt "Add to profile:") c (mkComplFunFromList' c ps) addCurrentWSToProfile

--------------------------------------------------------------------------------
addCurrentWSToProfile :: ProfileId -> X()
addCurrentWSToProfile pid = do
  cur <- gets $ W.tag . W.workspace . W.current . windowset
  addWSToProfile cur pid

--------------------------------------------------------------------------------
addWSToProfile :: WorkspaceId -> ProfileId -> X()
addWSToProfile wid pid = XS.modify go
  where
   go :: ProfileState -> ProfileState
   go ps = ps {profilesMap = update $ profilesMap ps, current = update' $ fromMaybe (Profile "default" []) $ current ps}

   update :: ProfileMap -> ProfileMap
   update mp = case Map.lookup pid mp of
     Nothing -> mp
     Just p  -> if wid `elem` profileWS p then mp else Map.adjust f pid mp

   f :: Profile -> Profile
   f p = Profile pid (wid : profileWS p) 

   update' :: Profile -> Maybe Profile
   update' cp = if profileId cp == pid && wid `notElem` profileWS cp then Just (Profile pid $ wid:profileWS cp) else Just cp

--------------------------------------------------------------------------------
removeWSFromProfilePrompt :: XPConfig -> X()
removeWSFromProfilePrompt c = do
  ps <- profileIds
  mkXPrompt (ProfilePrompt "Remove ws from profile:") c (mkComplFunFromList' c ps) f
  where
   f :: String -> X()
   f p = do
     arr <- profileWorkspaces p
     mkXPrompt (ProfilePrompt "Ws to remove from profile:") c (mkComplFunFromList' c arr) (`removeWSFromProfile` p)

--------------------------------------------------------------------------------
removeWSFromProfile :: WorkspaceId -> ProfileId -> X()
removeWSFromProfile wid pid = XS.modify go
  where
   go :: ProfileState -> ProfileState
   go ps = ps {profilesMap = update $ profilesMap ps, current = update' $ fromMaybe (Profile "default" []) $ current ps}

   update :: ProfileMap -> ProfileMap
   update mp = case Map.lookup pid mp of
     Nothing -> mp
     Just p  -> if wid `elem` profileWS p then Map.adjust f pid mp else mp

   f :: Profile -> Profile
   f p = Profile pid (delete wid $ profileWS p) 

   update' :: Profile -> Maybe Profile
   update' cp = if profileId cp == pid && wid `elem` profileWS cp then Just (Profile pid $ delete wid $ profileWS cp) else Just cp

--------------------------------------------------------------------------------
-- | Pretty printer for a bar. Prints workspace ids of a current profile.
excludeWSPP :: PP -> X PP
excludeWSPP pp = modifyPP <$> currentProfileWorkspaces
  where
    modifyPP pws = pp { ppRename = ppRename pp . printTag pws }
    printTag pws tag = if tag `elem` pws then tag else ""

--------------------------------------------------------------------------------
-- | For cycling through workspaces associated with the current
wsFilter :: WSType
wsFilter = WSIs $ currentProfileWorkspaces >>= (\ws -> return $ (`elem` ws) . W.tag)

--------------------------------------------------------------------------------
-- | Takes care of placing correct workspaces on their respective screens.
-- It does this by reducing the history of a Profile until it gets an array of length
-- equal to the number of screens with pairs that have unique workspace ids.
switchWSOnScreens :: ProfileId -> X()
switchWSOnScreens pid = do
  hist <- profileHistory
  vis <- gets $ W.visible . windowset
  cur <- gets $ W.current . windowset
  pws <- profileMap <&> (profileWS . fromMaybe (Profile pid []) . Map.lookup pid)
  case Map.lookup pid hist of
    Nothing -> switchScreens $ zip (W.screen <$> (cur:vis)) pws
    Just xs -> compareAndSwitch (f (W.screen <$> cur:vis) xs) (cur:vis) pws
  where
    f :: [ScreenId] -> [(ScreenId, WorkspaceId)] -> [(ScreenId, WorkspaceId)]
    f sids = reorderUniq . reorderUniq . reverse . filter ((`elem` sids) . fst)

    reorderUniq :: (Ord k, Ord v) => [(k,v)] -> [(v,k)]
    reorderUniq = map (\(x,y) -> (y,x)) . uniq

    uniq :: (Ord k, Ord v) => [(k,v)] -> [(k,v)]
    uniq = Map.toList . Map.fromList

    viewWS fview sid wid = windows $ fview sid wid

    switchScreens = mapM_ (uncurry $ viewWS greedyViewOnScreen)

    compareAndSwitch hist wins pws | length hist < length wins = switchScreens $ hist <> populateScreens hist wins pws
                                   | otherwise                 = switchScreens hist

    populateScreens hist wins pws = zip (filter (`notElem` map fst hist) $ W.screen <$> wins) (filter (`notElem` map snd hist) pws)

--------------------------------------------------------------------------------
chooseAction :: (String -> X ()) -> X ()
chooseAction f = XS.gets current <&> (profileId . fromMaybe (Profile "default" [])) >>= f

--------------------------------------------------------------------------------
-- | Keybindings per Profile
bindOn :: [(String, X ())] -> X ()
bindOn bindings = chooseAction chooser
  where
    chooser profile = case lookup profile bindings of
        Just action -> action
        Nothing -> case lookup "" bindings of
            Just action -> action
            Nothing -> return ()

--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
allProfileWindows :: XWindowMap
allProfileWindows = allProfileWindows' def

--------------------------------------------------------------------------------
allProfileWindows' :: WindowBringerConfig -> XWindowMap
allProfileWindows' WindowBringerConfig{ windowTitler = titler, windowFilter = include } = do
  pws <- currentProfileWorkspaces
  windowSet <- gets windowset
  Map.fromList . concat <$> mapM keyValuePairs (filter ((`elem` pws) . W.tag) $ W.workspaces windowSet)
   where keyValuePairs ws = let wins = W.integrate' (W.stack ws)
                           in mapM (keyValuePair ws) =<< filterM include wins
         keyValuePair ws w = (, w) <$> titler ws w
