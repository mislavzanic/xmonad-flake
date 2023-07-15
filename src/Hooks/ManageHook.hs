module Hooks.ManageHook where

import Workspaces.Scratchpad

import XMonad
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedScratchpad

activateHook :: ManageHook
activateHook = mconcat
    [ isDialog --> doAskUrgent
    , className =? "mpv" --> doAskUrgent
    , className =? "help" --> doAskUrgent
    , className =? "Zathura" --> doAskUrgent
    , className =? "Emacs" --> doAskUrgent
    , className =? "Brave-browser" --> doAskUrgent
    ]

myManageHook :: ManageHook
myManageHook = composeAll
    [ appName   =? "pavucontrol"      --> doCenterFloat
    , appName   =? "lxappearance"     --> doCenterFloat
    , className =? "Virt-manager"     --> doFloat
    , className =? "Thunderbird"      --> doFloat
    , className =? "Nvidia-settings"  --> doFloat

    , className =? "firefox"       --> doShift "web"
    , className =? "Brave-browser" --> doShift "brave"
    , className =? "Slack"         --> doShift "slack"

    , isFullscreen --> doFullFloat
    , namedScratchpadManageHook =<< liftX myScratchpads
    ]
