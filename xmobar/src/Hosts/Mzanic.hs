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
import Config.Helpers ( inColor, colorSeparator )
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
  topWidgets =
    [ Run cpu
    , Run memory
    , Run coreTemp
    , Run diskUsage
    , Run brightness
    , Run $ wireless "wlp2s0"
    , Run date
    , Run $ battery ["BAT0"]
    , Run $ XPropertyLog $ screenLog n
    ]

  bottomWidgets :: [Runnable]
  bottomWidgets =
    [ Run $ XPropertyLog $ screenLog n
    , Run $ XPropertyLog "_XMOBAR_HIDDEN_WIN"
    ] <>
    (if n == 0 then
      [Run trayer]
    else
      [Run date]
    )

myConfig :: Config -> Config
myConfig c = c 
  { font            = "Cantarell Bold 15"
  , additionalFonts = [ "FontAwesome 20"
                      , "Font Awesome 5 Free 20"
                      , "Font Awesome 5 Free Solid 20"
                      , "Font Awesome 5 Brands 20"
                      , "Inconsolata Nerd Font 20"
                      ]
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
                                                     , "%wlp2s0wi%"
                                                     , "%date%"
                                                     ]


getPosition :: Int -> String -> XPosition
getPosition n pos = case pos of
  "bottom" -> OnScreen n (BottomH 24)
  _        -> OnScreen n (TopH 24)

screenLog :: Int -> String
screenLog n = "_XMONAD_LOG_" <> show n
