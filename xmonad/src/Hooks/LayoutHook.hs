{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Hooks.LayoutHook where

import Data.Ratio

import Theme.Xprop


import XMonad

import XMonad.Hooks.ManageDocks
import XMonad.Layout.BoringWindows ( boringAuto )
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns ( ThreeCol(ThreeColMid) )
import XMonad.Layout.PerScreen (ifWider)
import XMonad.Layout.CenteredIfSingle (centeredIfSingle)
import XMonad.Actions.TopicSpace (TopicItem(tiName))
import XMonad.Layout.Hidden (hiddenWindows)
import XMonad.Util.UserConf (UserConf(userTopics, userFontStr))
import XMonad.Util.PTL

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

setName :: String -> l a -> ModifiedLayout Rename l a
setName n = renamed [Replace n]

rTall :: Int -> Rational -> Rational -> ResizableTall l
rTall m r c = ResizableTall m r c []

myLayout conf = onWorkspace "vid" full lh
  where
    lh = layoutOpts
       $ onWorkspaces hackingWorkspaces (hack ||| full)
       $ onWorkspaces tiledWorkspaces (tiled ||| full)
       $ tiled ||| hack ||| full
    layoutOpts = cutWords 2 . hiddenWindows . addTabbed . toggleBorders . avoidStruts

    wideScreen lay = ifWider 1920 (centeredIfSingle 0.7 1.0 lay) lay

    tcm = rn "ThreeColMid"
        $ mySpacing gapSize
        $ ThreeColMid 1 (3/100) (3/7)

    hack = rn "Hack"
         $ mySpacing gapSize
         $ limitWindows 3
         $ magnify 1.3 (NoMaster 3) True
         $ rTall 1 (3 % 100) (13 % 25)

    full = rn "Full" -- "Full"
         $ noBorders Full

    -- tiled
    tiled = rn "Tiled"--"Tall"
          $ mySpacing gapSize
          $ rTall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

    toggleBorders = mkToggle $ NBFULL ?? NOBORDERS ?? EOT
    addTabbed = boringAuto . addTabs shrinkText (tabTheme conf) . subLayout [] Simplest

    rn :: String -> l a -> ModifiedLayout Rename l a
    rn s = renamed [Replace s]

    gapSize :: Integer = 0

    hackingWorkspaces :: [String]
    hackingWorkspaces = map (tiName . topicItem) . filter (("hack" `elem`) . layouts) $ userTopics conf

    tiledWorkspaces :: [String]
    tiledWorkspaces = map (tiName . topicItem) . filter (("tiled" `elem`) . layouts) $ userTopics conf


data EmptyShrinker = EmptyShrinker deriving (Read, Show)
instance Shrinker EmptyShrinker where
    shrinkIt _ _ = []

cutWords :: Int -> l a -> ModifiedLayout Rename l a
cutWords i = renamed [CutWordsLeft i]

tabTheme :: UserConf -> Theme
tabTheme conf = def
  { activeColor = base01
  , urgentColor = base01
  , inactiveColor = basebg
  , activeBorderColor = base01
  , inactiveBorderColor = basebg
  , activeTextColor     = base00
  , inactiveTextColor   = base01
  , fontName = userFontStr conf
  }
