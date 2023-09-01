{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module XMonad.Actions.ProfileWindows where

import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

import XMonad.Actions.Profiles
import Data.List (sortBy, groupBy)
import XMonad.Hooks.StatusBar (xmonadPropLog')
import Theme.Xprop (base01)
import XMonad.Hooks.DynamicLog (xmobarColor)

type WinMap = Map (ProfileId, WorkspaceId) [Window]

newtype ProfileWindowMap = ProfileWindowMap
  { winMap :: WinMap
  }

instance ExtensionClass ProfileWindowMap where
  initialValue = ProfileWindowMap Map.empty

hideWindow :: Window -> X()
hideWindow win = modify (\s -> s { windowset = W.delete' win $ windowset s })

popHiddenWindow :: Window -> X()
popHiddenWindow = windows . W.insertUp

markWindow :: Window -> X ()
markWindow win = do
  p <- currentProfile
  cur <- gets $ W.tag . W.workspace . W.current . windowset
  XS.modify $ go (p,cur)
  where
   go :: (ProfileId, WorkspaceId) -> ProfileWindowMap -> ProfileWindowMap
   go (p,cur) pwm = pwm { winMap = update (p,cur) $ winMap pwm }

   update :: (ProfileId, WorkspaceId) -> WinMap -> WinMap
   update (p,cur) pwm = case Map.lookup (p,cur) pwm of
     Nothing -> Map.insert (p,cur) [win] pwm
     Just a  -> if win `notElem` a then Map.adjust (win:) (p,cur) pwm else pwm

unMarkWindow :: Window -> X ()
unMarkWindow win = do
  p <- currentProfile
  cur <- gets $ W.tag . W.workspace . W.current . windowset
  XS.modify $ go (p,cur)
  where
   go :: (ProfileId, WorkspaceId) -> ProfileWindowMap -> ProfileWindowMap
   go (p,cur) pwm = pwm { winMap = update (p,cur) $ winMap pwm }

   update :: (ProfileId, WorkspaceId) -> WinMap -> WinMap
   update (p,cur) pwm = case Map.lookup (p,cur) pwm of
     Nothing -> pwm
     Just a  -> if win `elem` a then Map.adjust (filter (/=win)) (p,cur) pwm else pwm

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

doActionOnWindows :: (Window -> X()) -> ProfileId -> X()
doActionOnWindows action pid = do
  pws <- profileWorkspaces pid
  mapM_ performAction pws
  where
    performAction :: WorkspaceId -> X()
    performAction wid = getWindows (pid,wid) >>= mapM_ action

    getWindows :: (ProfileId, WorkspaceId) -> X [Window]
    getWindows key = XS.gets winMap >>= \wm -> return $ fromMaybe [] $ Map.lookup key wm

hideMarkedWindows :: ProfileId -> X()
hideMarkedWindows = doActionOnWindows hideWindow

popMarkedWindows :: ProfileId -> X()
popMarkedWindows = doActionOnWindows popHiddenWindow

withCurrentProfile :: (ProfileId -> X()) -> X()
withCurrentProfile action = currentProfile >>= action
