-- |

module Config.Keys where

import Workspaces.Scratchpad
import Workspaces.Profile
import Config.Alias
import Config.Prompts
import Theme.Xprop

import qualified Data.Map as M

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO


import XMonad.Actions.CycleWS
import XMonad.Actions.Profiles
import XMonad.Actions.Promote ( promote )
import XMonad.Actions.Search ( promptSearch )
import XMonad.Actions.Submap ( visualSubmap )
import XMonad.Hooks.ManageDocks
import XMonad.Layout.BoringWindows
import XMonad.Layout.MultiToggle (Toggle (Toggle))
import XMonad.Layout.MultiToggle.Instances ( StdTransformers(NBFULL, NOBORDERS) )
import XMonad.Layout.SubLayouts ( mergeDir, GroupMsg(UnMerge), onGroup )
import XMonad.Prompt.Shell ( shellPrompt )
import XMonad.Prompt.Window
import XMonad.Util.XUtils
import XMonad.Util.NamedScratchpad ( namedScratchpadAction, scratchpadWorkspaceTag, toggleDynamicNSP, dynamicNSPAction )
import XMonad.Layout.Spacing (toggleWindowSpacingEnabled, toggleScreenSpacingEnabled, decWindowSpacing, decScreenSpacing, incWindowSpacing, incScreenSpacing)
import XMonad.Actions.Prefix (withPrefixArgument, PrefixArgument (Raw))
import XMonad.Actions.Minimize (minimizeWindow, withLastMinimized, maximizeWindowAndFocus)
import XMonad.Actions.PerProfileWindows (hideAll, showAll, hideFocused, showLastHidden, swapWithHidden)

type Keybind = (String, X ())

windowsKeys :: [Keybind]
windowsKeys =
  [ ("M-j", focusDown)
  , ("M-k", focusUp)
  , ("M-m", focusMaster)
  , ("M-S-j", swapDown)
  , ("M-S-k", swapUp)

  , ("M-C-j", onGroup W.focusDown')
  , ("M-C-k", onGroup W.focusUp')

  , ("M-w", nextScreen)
  , ("M-e", prevScreen)
  , ("M-C-w", swapNextScreen)
  , ("M-C-e", swapPrevScreen)

  , ("M-S-w", shiftNextScreen)
  , ("M-S-e", shiftPrevScreen)

  , ("M-<Backspace>", promote)

  , ("M-h", sendMessage Shrink)
  , ("M-l", sendMessage Expand)

  , ("M-;"  , withFocused $ sendMessage . mergeDir id)
  , ("M-C-;"  , withFocused (sendMessage . UnMerge) *> windows W.focusUp)
  ]

scratchpadKeys :: [(String, X ())]
scratchpadKeys =
  [ ("M-C-<Return>", namedScratchpadAction [] "Term")
  , ("M-C-s", namedScratchpadAction [] "Spotify")
  -- , ("M-C-t", namedScratchpadAction [] "org-todo")
  , ("M-C-u", namedScratchpadAction [] "Emacs")
  ]

promptKeys :: [(String, X ())]
promptKeys =
  [ ("M-p", shellPrompt promptTheme)
  , ("M-[", windowMultiPrompt promptTheme [(Goto, allProfileWindows), (Goto, wsWindows)])
  , ("M-]", windowPrompt promptTheme Bring allProfileWindows)
  , ("M1-s", visualSubmap winConfig $ searchList $ promptSearch promptTheme)

  , ("M-s", visualSubmap winConfig . M.fromList $
      [ ((noModMask, xK_f), ("Fullscreen screenshot", spawn "takeScreenshot.sh Fullscreen"))
      , ((noModMask, xK_r), ("Region screenshot", spawn "takeScreenshot.sh Region"))
      , ((noModMask, xK_a), ("Active window screenshot", spawn "takeScreenshot.sh Active Window"))
      ])
  , ("M-o", visualSubmap winConfig . M.fromList $
      [ ((noModMask, xK_h), ("Cro", chLang "hr"))
      , ((noModMask, xK_e), ("Eng", chLang "us"))
      ])
  ]
  where
   chLang :: String -> X()
   chLang lang = spawn ("setxkbmap " <> lang) >> spawn "xmodmap $XDG_CONFIG_HOME/x11/Xmodmap"

wsKeys :: [(String, X())]
wsKeys =
  [ ("M1-h", DO.swapWith Prev filterWS)
  , ("M1-l", DO.swapWith Next filterWS)
  , ("M1-j", DO.moveTo Next filterWS)
  , ("M1-k", DO.moveTo Prev filterWS)
  , ("M-C-p", switchProfilePrompt promptTheme)
  , ("M1-`", toggleLastProfile)
  ]
  where
    filterWS = wsFilter :&: Not emptyWS :&: ignoringWSs [scratchpadWorkspaceTag]

layoutKeys :: [Keybind]
layoutKeys =
  [ ("M-<Tab>",     sendMessage NextLayout)
  , ("M-C-<Space>", sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts)
  , ("M-C-b",       sendMessage $ Toggle NOBORDERS)
  , ("M-C-g",       toggleWindowSpacingEnabled >> toggleScreenSpacingEnabled)
  , ("M-C-[",       decWindowSpacing spacingWidth >> decScreenSpacing spacingWidth)
  , ("M-C-]",       incWindowSpacing spacingWidth >> incScreenSpacing spacingWidth)
  , ("M-<Space>",   withPrefixArgument $
                       \case Raw 1 -> sendMessage $ ToggleStrut D
                             Raw 2 -> sendMessage $ ToggleStrut U
                             _     -> sendMessage ToggleStruts
    )
  ]

appKeys :: [(String, X ())]
appKeys =
  [ ("M-q", kill)
  , ("M-C-q", withUnfocused killWindow)
  , ("M-S-r", spawn "xmonad --restart")

  , ("<XF86AudioRaiseVolume>", spawn "pamixer -i 5 && notify-send -u low -t 1500 $(pamixer --get-volume)")
  , ("<XF86AudioLowerVolume>", spawn "pamixer -d 5 && notify-send -u low -t 1500 $(pamixer --get-volume)")
  , ("<XF86AudioMute>", spawn "pamixer -t")

  , ("<XF86MonBrightnessDown>", spawn "brightnessctl s $(($(brightnessctl g) - 50))")
  , ("<XF86MonBrightnessUp>",   spawn "brightnessctl s $(($(brightnessctl g) + 50))")

  , ("M1-w", bindOn [("Work", spawn "brave"), ("", spawn "firefox")])

  , ("M-d", hideAll)
  , ("M-C-d", showAll)

  , ("M-f", hideFocused)
  , ("M-C-f", showLastHidden)

  , ("M-C-l", spawn "i3lock -n -i ~/.local/share/wallpaper")
  ]

myKeys :: String -> [(String, X ())]
myKeys host = concat
  [ appKeys
  , layoutKeys
  , promptKeys
  , windowsKeys
  , profileKeys host
  , wsKeys
  , scratchpadKeys
  ]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster)
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster)
    ]

winConfig :: WindowConfig
winConfig = WindowConfig { winBg = basebg
                         , winFg = basefg
                         , winFont = myFont
                         , winRect = CustomRect Rectangle { rect_y = -40
                                                          , rect_x = 1600
                                                          , rect_width = 350
                                                          , rect_height = 430
                                                          }
                         }
