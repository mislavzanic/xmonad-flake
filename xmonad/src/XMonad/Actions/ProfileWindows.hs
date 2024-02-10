{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module XMonad.Actions.ProfileWindows where

import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.IO (hPutStrLn, stderr)

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import Data.Foldable (forM_)

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

addProfileWindows :: XConfig a -> XConfig a
addProfileWindows conf = conf { logHook = borderColorLogHook <> logHook conf }

borderColorLogHook :: X()
borderColorLogHook = do
  p <- currentProfile
  pwm <- winMap <$> XS.get
  cur <- gets $ W.tag . W.workspace . W.current . windowset
  forM_ (Map.lookup (p, cur) pwm) $ mapM_ $ \win -> withFocused $ \fwin -> do
    case win == fwin of
      True  -> changeColor "#ff33ff" win
      False -> changeColor "#331133" win

changeColor :: String -> Window -> X()
changeColor cs win = withDisplay $ \dpy -> do
    c' <- io (initColor dpy cs)
    forM_ c' $ setWindowBorderWithFallback dpy win cs

hideWindow' :: WorkspaceId -> Window -> X()
hideWindow' _ win = windows $ W.delete' win

hideWindow :: Window -> X()
hideWindow = hideWindow' ""

popHiddenWindow :: WorkspaceId -> Window -> X()
popHiddenWindow wid = windows . insertToWorkspace wid

with :: b -> (W.Stack a -> b) -> W.StackSet i l a s sd -> b
with dflt f = maybe dflt f . W.stack . W.workspace . W.current

insertToWorkspace :: Eq i => Eq a => i -> a -> W.StackSet i l a s sd -> W.StackSet i l a s sd
insertToWorkspace wid a s = if W.member a s then s else insert
  where
   insert
     | wid == (W.tag . W.workspace . W.current $ s) = insertCurrent $ (\(W.Stack t l r) -> Just $ W.Stack a l (t:r))
     | wid `elem` (W.tag . W.workspace <$> W.visible s) = insertVisible $ (\(W.Stack t l r) -> Just $ W.Stack a l (t:r))
     | wid `elem` (W.tag <$> W.hidden s) = insertHidden $ (\(W.Stack t l r) -> Just $ W.Stack a l (t:r))
     | otherwise = insertCurrent $ (\(W.Stack t l r) -> Just $ W.Stack a l (t:r))
   insertCurrent f = W.modify (Just $ W.Stack a [] []) f s
   insertVisible f = s { W.visible = (\(W.Screen w scr scrd) -> if wid == W.tag w then (W.Screen w scr scrd) { W.workspace = w
                                                                                             { W.stack = f (case W.stack w of
                                                                                                              Just st -> st
                                                                                                              Nothing -> W.Stack a [] [])
                                                                                             }
                                                                                           } else W.Screen w scr scrd) <$> W.visible s }
   insertHidden  f = s { W.hidden = (\w -> if wid == W.tag w then w { W.stack = f (case W.stack w of
                                                                                     Just st -> st
                                                                                     Nothing -> W.Stack a [] [])
                                                                    } else w) <$> W.hidden s }


markWindow :: Window -> X ()
markWindow win = do
  p <- currentProfile
  cur <- gets $ W.tag . W.workspace . W.current . windowset
  XS.modify $ go (p,cur)
  changeColor "#ff33ff" win
  where
   go :: (ProfileId, WorkspaceId) -> ProfileWindowMap -> ProfileWindowMap
   go (p,cur) pwm = pwm { winMap = update (p,cur) $ winMap pwm }

   update :: (ProfileId, WorkspaceId) -> WinMap -> WinMap
   update (p,cur) pwm = case Map.lookup (p,cur) pwm of
     Nothing -> Map.insert (p,cur) [win] pwm
     Just a  -> if win `notElem` a then Map.adjust (win:) (p,cur) pwm else pwm

unMarkWindow :: Window -> X ()
unMarkWindow win = do
  XConf { config = conf } <- ask
  p <- currentProfile
  cur <- gets $ W.tag . W.workspace . W.current . windowset
  XS.modify $ go (p,cur)
  changeColor (focusedBorderColor conf) win
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

doActionOnWindows :: (WorkspaceId -> Window -> X()) -> ProfileId -> X()
doActionOnWindows action pid = do
  pws <- profileWorkspaces pid
  mapM_ performAction pws
  where
    performAction :: WorkspaceId -> X()
    performAction wid = getWindows (pid,wid) >>= mapM_ (action wid)

    getWindows :: (ProfileId, WorkspaceId) -> X [Window]
    getWindows key = XS.gets winMap >>= \wm -> return $ fromMaybe [] $ Map.lookup key wm

hideMarkedWindows :: ProfileId -> X()
hideMarkedWindows = doActionOnWindows hideWindow'

popMarkedWindows :: ProfileId -> X()
popMarkedWindows = doActionOnWindows popHiddenWindow

withCurrentProfile :: (ProfileId -> X()) -> X()
withCurrentProfile action = currentProfile >>= action
