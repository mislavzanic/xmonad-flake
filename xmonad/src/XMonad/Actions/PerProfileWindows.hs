{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module XMonad.Actions.PerProfileWindows
  ( hideAll
  , showAll
  , hideBeforeSwitch
  , showAfterSwitch
  , hiddenWSLogHook
  , hideFocused
  , showLastHidden
  , swapWithHidden
  ) where

import Control.Monad
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

import XMonad.Actions.Profiles
import XMonad.Layout.Hidden (hideWindow, popHiddenWindow)
import Data.List (intersect, sortBy, groupBy)
import XMonad.Hooks.StatusBar (xmonadPropLog')
import Theme.Xprop (base01)
import XMonad.Hooks.DynamicLog (xmobarColor)

newtype ProfileWindowMap = ProfileWindowMap
  { winMap :: Map (ProfileId, WorkspaceId) [Window]
  }

instance ExtensionClass ProfileWindowMap where
  initialValue = ProfileWindowMap Map.empty

storeWins :: ProfileId -> WorkspaceId -> [Window] -> X()
storeWins pid wid wins = XS.modify go
  where
    go s = s { winMap = update $ winMap s }
    update = Map.alter f (pid,wid)
    f value = case value of
      Nothing -> Just wins
      Just v  -> Just $ v <> wins

updateMap :: ProfileId -> WorkspaceId -> [Window] -> X()
updateMap pid wid wins = XS.modify go
  where
    go s = s { winMap = update $ winMap s }
    update = Map.alter (const $ Just wins) (pid,wid)

hiddenWSLogHook :: X()
hiddenWSLogHook = xmonadPropLog' "_XMOBAR_HIDDEN_WIN" =<< allHiddenWindows
  where
    addSep str = if str == "" then str else xmobarColor base01 "" "<fn=5>\xf070</fn>       " <> str <> xmobarColor "#586E75" "" "  |  "
    allHiddenWindows = do
      wm <- map (\xs -> (fst . head $ xs, map snd xs))
          . groupBy (\x y -> (fst x :: String) == fst y)
          . sortBy (\x y -> compare (fst x :: String) (fst y))
          . Map.keys
          . Map.filter (/= ([] :: [Window])) <$> XS.gets winMap
      return . addSep
             . xmobarColor base01 ""
             . foldl (\acc str -> acc <> "[" <> str <> "]") ""
             . map (\(pid, wids) -> pid <> ":" <> foldl (\acc wid -> acc <> " " <> wid) "" wids) $ wm

getWindows :: WorkspaceId -> X [Window]
getWindows wid = do
  ws:_ <- gets $ filter ((==wid) . W.tag) .  W.workspaces . windowset
  return $ maybe [] W.integrate (W.stack ws)


withCurrentPW :: (ProfileId -> WorkspaceId -> X()) -> X()
withCurrentPW action = do
  p <- currentProfile
  t <- gets $ W.currentTag . windowset
  action p t

hideBeforeSwitch :: ProfileId -> X()
hideBeforeSwitch next = do
  current <- currentProfile
  wss <- wsIntersection current next
  mapM_ (uncurry hideAll') wss
  where
    wsIntersection p1 p2 = do
      pws1 <- profileWorkspaces p1
      pws2 <- profileWorkspaces p2
      return $ map (p1,) $ intersect pws1 pws2

hideFocused :: X()
hideFocused = withCurrentPW hideFocused'

hideFocused' :: ProfileId -> WorkspaceId -> X()
hideFocused' pid wid = withFocused recordAndHide
  where
    recordAndHide win = storeWins pid wid [win] >> hideWindow win

hideAll :: X()
hideAll = withCurrentPW hideAll'

hideAll' :: ProfileId -> WorkspaceId -> X()
hideAll' pid wid = getWindows wid >>= recordWindows
  where
    recordWindows wins = unless (null wins) (storeWins pid wid wins >> mapM_ hideWindow wins)

getWindowsFromWinMap :: ProfileId -> WorkspaceId -> X [Window]
getWindowsFromWinMap pid wid = XS.gets winMap >>= returnWindow
  where
    returnWindow wm = case Map.lookup (pid,wid) wm of
      Nothing -> return []
      Just wins -> return wins

showAfterSwitch :: ProfileId -> X()
showAfterSwitch prev = do
  current <- currentProfile
  wss <- wsIntersection current prev
  mapM_ (uncurry showAll') wss
  where
    wsIntersection p1 p2 = do
      pws1 <- profileWorkspaces p1
      pws2 <- profileWorkspaces p2
      return $ map (p1,) $ intersect pws1 pws2

showLastHidden :: X()
showLastHidden = withCurrentPW showLastHidden'

showLastHidden' :: ProfileId -> WorkspaceId -> X()
showLastHidden' pid wid = withLastWin popHiddenWindow
  where
    lookupWins :: Map (ProfileId, WorkspaceId) [Window] -> [Window]
    lookupWins wm = fromMaybe [] (Map.lookup (pid,wid) wm)

    withLastWin :: (Window -> X()) -> X()
    withLastWin action = do
      ws <- lookupWins <$> XS.gets winMap
      unless (null ws) (updateMap pid wid (init ws) >> action (last ws))

showAll :: X()
showAll = withCurrentPW showAll'

showAll' :: ProfileId -> WorkspaceId -> X()
showAll' pid wid = getWindowsFromWinMap pid wid >>= removeWindows
  where
    removeWindows wins = unless (null wins) (XS.modify go >> mapM_ (\w -> popHiddenWindow w >> windows (W.shiftWin wid w)) (reverse wins))
    go s = s { winMap = update $ winMap s }
    update = Map.alter (const Nothing) (pid,wid)

swapWithHidden :: X()
swapWithHidden = withCurrentPW swapWithHidden'

swapWithHidden' :: ProfileId -> WorkspaceId -> X()
swapWithHidden' pid wid = do
  vis <- getWindows wid
  hidden <- getWindowsFromWinMap pid wid
  hideVisible vis
  showHidden hidden
  where
    showHidden wins = unless (null wins) (XS.modify (go wins) >> mapM_ (\w -> popHiddenWindow w >> windows (W.shiftWin wid w)) (reverse wins))
    go wins s = s { winMap = update wins $ winMap s }
    update wins = Map.alter (f wins) (pid,wid)
    f wins val = case val of
      Nothing -> Nothing
      Just v -> Just . filter (`notElem` wins) $ v
    hideVisible wins = unless (null wins) (storeWins pid wid wins >> mapM_ hideWindow wins)
