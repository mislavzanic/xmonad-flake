module Config.Keys where

import Workspaces.Profile
import Config.Prompts
import Theme.Xprop

import qualified Data.Map as M

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO

import XMonad.Prompt
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
import XMonad.Layout.PerProfileWindows
import XMonad.Actions.Prefix (withPrefixArgument, PrefixArgument (Raw))
import XMonad.Actions.Minimize (minimizeWindow, withLastMinimized, maximizeWindowAndFocus)
import XMonad.Actions.PerProfileWindows (hideAll, showAll, hideFocused, showLastHidden, swapWithHidden)
import XMonad.Util.UserConf

type Keybind = (String, X ())

windowsKeys :: [Keybind]
windowsKeys =
  [ ("M-j",   focusDown)
  , ("M-k",   focusUp)
  , ("M-m",   focusMaster)
  , ("M-S-j", swapDown)
  , ("M-S-k", swapUp)

  , ("M-C-j", onGroup W.focusDown')
  , ("M-C-k", onGroup W.focusUp')

  , ("M-w",   nextScreen)
  , ("M-e",   prevScreen)
  , ("M-C-w", swapNextScreen)
  , ("M-C-e", swapPrevScreen)

  , ("M-S-w", shiftNextScreen)
  , ("M-S-e", shiftPrevScreen)

  , ("M-<Backspace>", promote)

  , ("M-h", sendMessage Shrink)
  , ("M-l", sendMessage Expand)

  , ("M-;", withPrefixArgument $
              \case Raw 1 -> withFocused (sendMessage . UnMerge) *> windows W.focusUp
                    _     -> withFocused $ sendMessage . mergeDir id)
  ]

scratchpadKeys :: [(String, X ())]
scratchpadKeys =
  [ ("M-C-<Return>", namedScratchpadAction [] "Term")
  , ("M-C-s",        namedScratchpadAction [] "Spotify")
  , ("M-C-u",        namedScratchpadAction [] "Emacs")
  ]

promptKeys :: UserConf -> [(String, X ())]
promptKeys conf =
  [ ("M-p", withPrefixArgument $
              \case Raw 1 -> switchProfilePrompt' promptTheme
                    Raw 2 -> addWSToProfilePrompt promptTheme
                    Raw 3 -> removeWSFromProfilePrompt promptTheme
                    _     -> shellPrompt promptTheme)
  , ("M-n", withPrefixArgument $
              \case Raw 1 -> withFocused unMarkWindow
                    _     -> withFocused markWindow)
  , ("M-d",  withPrefixArgument $
               \case Raw 1 -> windowPrompt promptTheme Bring allProfileWindows
                     _     -> windowMultiPrompt promptTheme [(Goto, allProfileWindows), (Goto, wsWindows)])
  , ("M-[",  windowMultiPrompt promptTheme [(Goto, allProfileWindows), (Goto, wsWindows)])
  , ("M-]",  windowPrompt promptTheme Bring allProfileWindows)
  , ("M1-s", visualSubmap winConf $ searchList $ promptSearch promptTheme)

  , ("M-s", visualSubmap winConf . M.fromList $
      [ ((noModMask, xK_f), ("Fullscreen screenshot", spawn $ screenshotProg <> " Fullscreen"))
      , ((noModMask, xK_r), ("Region screenshot", spawn $ screenshotProg <> " Region"))
      , ((noModMask, xK_a), ("Active window screenshot", spawn $ screenshotProg <> " Active Window"))
      ])
  , ("M-o", visualSubmap winConf . M.fromList $
      [ ((noModMask, xK_h), ("Cro", chLang "hr"))
      , ((noModMask, xK_e), ("Eng", chLang "us"))
      ])
  ]
  where
   chLang :: String -> X()
   chLang lang = spawn ("setxkbmap " <> lang <> " && xmodmap " <> userConfDir conf <> "/x11/Xmodmap &")

   winConf = winConfig conf

   screenshotProg = userConfDir conf <> "/xmonad/" <> "screenshot"

   promptTheme :: XPConfig
   promptTheme = userPromptConfig conf

wsKeys :: UserConf -> [(String, X())]
wsKeys conf =
  [ ("M1-h",  DO.swapWith Prev filterWS)
  , ("M1-l",  DO.swapWith Next filterWS)
  , ("M1-j",  DO.moveTo Next filterWS)
  , ("M1-k",  DO.moveTo Prev filterWS)
  , ("M1-`",  toggleLastProfile)
  ]
  where
    filterWS = wsFilter :&: Not emptyWS :&: ignoringWSs [scratchpadWorkspaceTag]

layoutKeys :: UserConf -> [Keybind]
layoutKeys conf =
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
  where
   spacingWidth = userSpacingWidth conf

appKeys :: UserConf -> [(String, X ())]
appKeys conf =
  [ ("M-q", withFocused unMarkWindow >> kill)

  , ("<XF86AudioRaiseVolume>", spawn "pamixer -i 5 && notify-send -u low -t 1500 $(pamixer --get-volume)")
  , ("<XF86AudioLowerVolume>", spawn "pamixer -d 5 && notify-send -u low -t 1500 $(pamixer --get-volume)")
  , ("<XF86AudioMute>", spawn "pamixer -t")

  , ("<XF86MonBrightnessDown>", spawn "brightnessctl s $(($(brightnessctl g) - 50))")
  , ("<XF86MonBrightnessUp>",   spawn "brightnessctl s $(($(brightnessctl g) + 50))")

  , ("M1-w", spawn $ userBrowser conf)

  , ("M-C-l", spawn $ userLock conf)
  ]

myKeys :: UserConf -> [(String, X ())]
myKeys conf = concat
  [ appKeys conf
  , layoutKeys conf
  , promptKeys conf
  , wsKeys conf
  , windowsKeys
  , profileKeys conf
  , scratchpadKeys
  ]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster)
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster)
    ]

winConfig :: UserConf -> WindowConfig
winConfig conf = WindowConfig
  { winBg = basebg
  , winFg = basefg
  , winFont = userFontStr conf
  , winRect = CustomRect Rectangle { rect_y = -40
                                   , rect_x = 1600
                                   , rect_width = 350
                                   , rect_height = 430
                                   }
  }
