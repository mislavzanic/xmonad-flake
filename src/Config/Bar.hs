{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans            #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans #-}

module Config.Bar where

import Theme.Xprop

import Config.Alias ( fnNerd, fnAwe )

import Data.List
import Text.Regex

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.PureX as P

import XMonad.Actions.Profiles

import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Actions.DynamicWorkspaceOrder (getSortByOrder)

toIcons :: String -> String
toIcons = s "web" (fnAwe "\xf269")
        . s "slack" (fnAwe "\xf198")
        -- . s "k9s" (fnAwe "\xf0c2")
  where
    s re sub x = subRegex (mkRegex re) x sub

baseXmobarConf :: PP
baseXmobarConf = def
  { ppCurrent = xmobarColor base01 "" . wrap "[" "]"
  , ppVisible = xmobarColor base01 ""
  , ppUrgent  = xmobarColor  base01 "" . wrap "!" "!"  -- Urgent workspace
  , ppHidden  = xmobarColor base08 ""
  , ppTitleSanitize = xmobarStrip
  , ppSep = wrapInColor "#586E75" "  |  "
  , ppSort = getSortByOrder
  , ppRename = ppRename def . toIcons
  }
  where
    wrapInColor :: String -> String -> String
    wrapInColor color toWrap = "<fc=" ++ color ++ ">" ++ toWrap ++ "</fc>"

xmobar' :: String -> [String] -> X String -> StatusBarConfig
xmobar' prop args pp = statusBarGeneric cmd $ xmonadPropLog' prop =<< pp
  where
    cmd = "bar " ++ unwords args

xmobar :: String -> [String] -> X PP -> StatusBarConfig
xmobar prop args pp = xmobar' prop args (dynamicLogString =<< pp)

xmobarTop :: String -> StatusBarConfig
xmobarTop hostname = xmobar "_XMONAD_LOG_2" ["-x", "2", "-h", hostname, "-p", "top"] (xmobarTopPP >>= excludeWSPP)

xmobarBottom :: ScreenId -> String -> StatusBarConfig
xmobarBottom sid@(S (show -> sn)) hostname = statusBarPropTo prop args (xmobarBottomPP sid)-- prop args
        where
          prop = "_XMONAD_LOG_" <> sn
          args = "bar -x "  <> sn <> " -h " <> hostname <> " -p bottom"

barSpawner :: String -> ScreenId -> StatusBarConfig
barSpawner hostname sid = case sid of
  0 -> xmobarTop hostname <> xmobarBottom 0 hostname
  1 -> xmobarBottom 1 hostname
  _ -> mempty

xmobarTopPP :: X PP
xmobarTopPP = pure $ filterOutWsPP [scratchpadWorkspaceTag] $ baseXmobarConf
  { ppSep = wrapInColor "#586E75" "  ::  "
  , ppExtras = [profileLogger formatFocused formatUnfocused]
  , ppOrder = \(ws:_:_:p) -> ws:p
  }
  where
    wrapInColor :: String -> String -> String
    wrapInColor color toWrap = "<fc=" ++ color ++ ">" ++ toWrap ++ "</fc>"

    formatFocused = xmobarColor base01 "" . wrap "[" "]"
                  . formatIcons

    formatUnfocused = xmobarColor base08 ""
                    . formatIcons

    formatIcons = s "Dots" (".X" <> fnNerd "\xe777" <> " ")
                -- . s "Dev" (fnAwe "\xf121")
                -- . s "Media" (fnAwe "\xf16a")
                -- . s "Work" (fnAwe "\xf0b1")

    s re sub x = subRegex (mkRegex re) x sub

xmobarBottomPP :: ScreenId -> X PP
xmobarBottomPP sid = pure $ baseXmobarConf
  { ppExtras =
      [ currentWS
      , logTitlesOnScreen sid formatFocused formatUnfocused
      ]
  , ppOrder = \(_:l:_:ws:xs) -> [ws, l] ++ xs
  }
  where
    focusColor, unFocusColor :: String -> String
    focusColor = xmobarColor base01 "" . wrap "[" "]"
    unFocusColor = xmobarColor base08 "" . wrap "[" "]"

    formatFocused, formatUnfocused :: String -> String
    formatFocused   = focusColor . ppWindow
    formatUnfocused = unFocusColor . ppWindow

    currentWS :: X(Maybe String)
    currentWS = withScreen $ \scr -> do
      curTag <- gets $ W.currentTag . windowset
      let ws = W.workspace scr
      let tag = W.tag ws
      return $ Just $ if curTag == tag then focusColor (toIcons tag) else xmobarColor base08 "" (toIcons tag)

    withScreen f = do
      s <- gets (find ((sid ==) . W.screen) . W.screens . windowset)
      P.whenJust' s f

    ppWindow :: String -> String
    ppWindow = xmobarRaw
           . (\w -> if null w then "untitled" else w)
           . shorten 30
           . xmobarStrip
