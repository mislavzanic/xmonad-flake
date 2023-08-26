{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeSynonymInstances, PatternGuards #-}

module XMonad.Layout.PerProfileWindows where

import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import XMonad
import XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet as W
import XMonad.Util.XUtils
import XMonad.Actions.Profiles

newtype ProfileWindowMap a = ProfileWindowMap (Map (ProfileId, WorkspaceId) [Window]) deriving (Show, Read)
  
data PHiddenMsg = MarkWindow Window
                | UnMarkWindow Window
                | ToggleMarked
                | HideMarked
                | PopMarked
                deriving (Eq)

instance Message PHiddenMsg

instance LayoutModifier ProfileWindowMap Window where
  handleMess h@(ProfileWindowMap pHidden) mess
    | Just (MarkWindow win)   <- fromMessage mess = markWindowMsg h win
    | Just (UnMarkWindow win) <- fromMessage mess = unMarkWindowMsg h win
    | Just HideMarked         <- fromMessage mess = hideMarkedMsg h
    | Just PopMarked          <- fromMessage mess = popMarkedMsg h
    -- | Just ToggleMarked       <- fromMessage mess = toggleMarkedMsg h
    | otherwise                                  = return Nothing

  modifierDescription _ = "PHidden"

pHiddenWindows :: LayoutClass l Window => l Window -> ModifiedLayout ProfileWindowMap l Window
pHiddenWindows = ModifiedLayout $ ProfileWindowMap Map.empty

markWindow :: Window -> X()
markWindow = sendMessage . MarkWindow

unMarkWindow :: Window -> X()
unMarkWindow = sendMessage . UnMarkWindow

hideMarkedWindows :: X()
hideMarkedWindows = sendMessage HideMarked

popMarkedWindows :: X()
popMarkedWindows = sendMessage PopMarked

markWindowMsg :: ProfileWindowMap a -> Window -> X (Maybe (ProfileWindowMap a))
markWindowMsg (ProfileWindowMap pMap) win = do
  p <- currentProfile
  cur <- gets $ W.tag . W.workspace . W.current . windowset
  return . Just . ProfileWindowMap $ update (p,cur)
  where
   update :: (ProfileId, WorkspaceId) -> Map (ProfileId, WorkspaceId) [Window]
   update (p,cur) = case Map.lookup (p,cur) pMap of
     Nothing -> Map.insert (p,cur) [win] pMap
     Just v  -> if win `notElem` v then Map.adjust f (p,cur) pMap else pMap

   f :: [Window] -> [Window]
   f ws = win:ws

unMarkWindowMsg :: ProfileWindowMap a -> Window -> X (Maybe (ProfileWindowMap a))
unMarkWindowMsg (ProfileWindowMap pMap) win = do
  p <- currentProfile
  cur <- gets $ W.tag . W.workspace . W.current . windowset
  return . Just . ProfileWindowMap $ update (p,cur)
  where
   update :: (ProfileId, WorkspaceId) -> Map (ProfileId, WorkspaceId) [Window]
   update (p,cur) = case Map.lookup (p,cur) pMap of
     Nothing -> pMap
     Just v  -> if win `elem` v then Map.adjust f (p,cur) pMap else pMap

   f :: [Window] -> [Window]
   f = filter (/= win)
  

hideMarkedMsg :: ProfileWindowMap a -> X (Maybe (ProfileWindowMap a))
hideMarkedMsg (ProfileWindowMap pMap) = do
  p <- currentProfile
  pws <- currentProfileWorkspaces
  mapM_ (hideMarkedWindows p) pws
  return . Just . ProfileWindowMap $ pMap
  where
   hideMarkedWindows :: ProfileId -> WorkspaceId -> X()
   hideMarkedWindows pid wid =
     mapM_ (\win -> modify (\s -> s { windowset = W.delete' win $ windowset s })) $ getWindows pid wid

   getWindows pid wid = fromMaybe [] $ Map.lookup (pid,wid) pMap


popMarkedMsg :: ProfileWindowMap a -> X (Maybe (ProfileWindowMap a))
popMarkedMsg (ProfileWindowMap pMap) = do
  p <- currentProfile
  pws <- currentProfileWorkspaces
  mapM_ (popMarkedWindows p) pws
  return . Just . ProfileWindowMap $ pMap
  where
   popMarkedWindows :: ProfileId -> WorkspaceId -> X()
   popMarkedWindows pid wid =
     mapM_ (windows . W.insertUp) $ getWindows pid wid

   getWindows pid wid = fromMaybe [] $ Map.lookup (pid,wid) pMap

sc :: String -> Window -> X ()
sc colorName win = withDisplay $ \dpy ->
  stringToPixel dpy colorName >>= setWindowBorderWithFallback dpy win colorName 
