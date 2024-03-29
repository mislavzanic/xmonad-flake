module Workspaces.Topics
  ( appendToName
  , spawnTermInTopic
  , spawnEditorInTopic
  , goto
  , toggleTopic
  , switchNthLastFocusedByScreen
  )where

import Data.Maybe ( fromMaybe )

import XMonad
import XMonad.Actions.Profiles
import XMonad.Actions.TopicSpace hiding ( switchNthLastFocusedByScreen, workspaceHistoryByScreen )
import XMonad.Prompt
import qualified XMonad.StackSet as W
import XMonad.Util.Run
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

defaultProfileTopic :: String -> String
defaultProfileTopic "Work" = "brave"
defaultProfileTopic _ = "web"
