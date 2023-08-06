module Workspaces.Topics
  ( ProfileItem(..)
  , appendToName
  , mkItem
  , spawnTermInTopic
  , spawnEditorInTopic
  , goto
  , toggleTopic
  , switchNthLastFocusedByScreen
  , switchProfileTopicPrompt
  , shiftProfileTopicPrompt
  )where

import Control.Monad
import Data.Maybe ( fromMaybe )

import XMonad
import XMonad.Actions.Profiles
import XMonad.Actions.TopicSpace hiding ( switchNthLastFocusedByScreen, workspaceHistoryByScreen )
import XMonad.Prompt
import qualified XMonad.StackSet as W
import XMonad.Util.Run
import XMonad.Util.UserConf
import qualified Data.Map as Map

appendToName :: ProfileId -> TopicItem -> TopicItem
appendToName p ti = TI (tiName ti <> ":" <> p) (tiDir ti) (tiAction ti)

spawnTermInTopic :: TopicConfig -> X ()
spawnTermInTopic tc = proc $ termInDir >-$ currentTopicDir tc

spawnEditorInTopic :: TopicConfig -> X ()
spawnEditorInTopic tc = proc $ inEditor >-$ currentTopicDir tc

goto :: TopicConfig -> Topic -> X ()
goto = switchTopic 

toggleTopic :: TopicConfig -> X ()
toggleTopic tc = switchNthLastFocusedByScreen tc 1

switchNthLastFocusedByScreen :: TopicConfig -> Int -> X ()
switchNthLastFocusedByScreen tc depth = do
  sid <- gets $ W.screen . W.current . windowset
  p   <- currentProfile
  sws <- map snd
       . filter ((==sid) . fst)
       . fromMaybe []
       . Map.lookup p <$> profileHistory
  switchTopic tc $ (sws ++ repeat (defaultProfileTopic p)) !! depth

data PfTopicPrompt = PfTopicPrompt XPConfig ProfileTopicMode [String]
data ProfileTopicMode = SwitchMode | ShiftMode

instance XPrompt PfTopicPrompt where
  showXPrompt (PfTopicPrompt _ submode _) =
    case submode of
      SwitchMode -> "Switch to Topic: "
      ShiftMode  -> "Send Window to Topic: "
  completionFunction (PfTopicPrompt c _ ns) = mkComplFunFromList' c ns

switchProfileTopicPrompt :: TopicConfig -> XPConfig -> X ()
switchProfileTopicPrompt tc c = mkPrompt =<< currentProfileWorkspaces
  where
    mkPrompt pws = mkXPrompt (PfTopicPrompt c SwitchMode pws) c (mkComplFunFromList' c pws) mbygoto 
    mbygoto wid = do
      pw <- profileWorkspaces =<< currentProfile
      unless (wid `notElem` pw) (goto tc wid)

shiftProfileTopicPrompt :: XPConfig -> X ()
shiftProfileTopicPrompt c = mkPrompt =<< currentProfileWorkspaces
  where
    mkPrompt pws = mkXPrompt (PfTopicPrompt c ShiftMode pws) c (mkComplFunFromList' c pws) mbyshift
    mbyshift wid = do
      pw <- profileWorkspaces =<< currentProfile
      unless (wid `notElem` pw) (windows . W.shift $ wid)

defaultProfileTopic :: String -> String
defaultProfileTopic "Work" = "brave"
defaultProfileTopic _ = "web"
