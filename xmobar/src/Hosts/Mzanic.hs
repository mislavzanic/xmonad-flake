{-# LANGUAGE PostfixOperators    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hosts.Mzanic where

import Xmobar
    ( defaultConfig,
      Config(font, additionalFonts, bgColor, borderColor, fgColor,
             position, lowerOnStart, hideOnStart, allDesktops, persistent,
             iconRoot, sepChar, alignSep, commands, template),
      XPosition(TopH, BottomH, OnScreen),
      Command(Com),
      Date(Date),
      Monitors(Wireless, Cpu, Memory, CoreTemp, DiskU, Brightness),
      XMonadLog(XPropertyLog),
      Runnable(..) )

import Data.List ( intercalate )
import Config.Helpers ( inColor, colorSeparator, screenLog )
import Config.BarConfig
import Config.Widgets
    ( battery,
      date,
      wireless,
      brightness,
      diskUsage,
      coreTemp,
      memory,
      cpu,
      trayer )

myCommands :: Int -> String -> [Runnable]
myCommands n pos = case pos of
  "bottom" -> bottomWidgets
  _        -> topWidgets
  where
  topWidgets :: [Runnable]
  topWidgets = (baseTopWidgets n pos) <>
    [ Run coreTemp
    , Run brightness
    , Run $ wireless "wlp0s20f3"
    ]

  bottomWidgets :: [Runnable]
  bottomWidgets = baseBottomWidgets n pos

myConfig :: Int -> String -> Config -> Config
myConfig screenId pos c = c 
  { font            = "Cantarell Bold 16"
  , additionalFonts = [ "FontAwesome 20"
                      , "Font Awesome 5 Free 20"
                      , "Font Awesome 5 Free Solid 20"
                      , "Font Awesome 5 Brands 20"
                      , "Inconsolata Nerd Font 20"
                      ]
  , position        = getPosition screenId pos 32
  }

myTemplate :: Int -> String -> String
myTemplate n pos = case pos of
  "bottom" -> bottomTemplate
  _        -> topTemplate
  where
    bottomTemplate :: String = "%" <> screenLog n <> "% }{" <> "%_XMOBAR_HIDDEN_WIN%" <> (if n == 0 then "%trayerpad%" else "%date%")
    topTemplate :: String = intercalate (colorSeparator "|") leftWidgets <> myScreenLog <> intercalate (colorSeparator "|") rightWidgets
    myScreenLog :: String = "} <icon=haskell.xpm/>" <> colorSeparator "::" <> "%" <> "_XMONAD_LOG_" <> show n <> "% { "
    leftWidgets :: [String] = inColor "#cccccc" <$> [ "%battery%"
                                                    , "%bright%"
                                                    , "%cpu%"
                                                    , "%memory%"
                                                    ]
    rightWidgets :: [String] = inColor "#cccccc" <$> [ "%coretemp%"
                                                     , "%disku%"
                                                     , "%wlp0s20f3wi%"
                                                     , "%date%"
                                                     ]
