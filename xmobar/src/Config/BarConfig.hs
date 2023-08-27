{-# LANGUAGE PostfixOperators    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Config.BarConfig
  ( baseConfig
  , baseTopWidgets
  , baseBottomWidgets
  , getPosition
  ) where

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

import Xmobar hiding (date)
import Config.Helpers

baseConfig :: Int -> String -> Config
baseConfig screenId pos = defaultConfig
  { font            = "Cantarell Bold 8"
  , additionalFonts = [ "FontAwesome 12"
                      , "Font Awesome 5 Free 12"
                      , "Font Awesome 5 Free Solid 12"
                      , "Font Awesome 5 Brands 12"
                      , "Inconsolata Nerd Font 12"
                      ]
  , bgColor         = myppBgColor
  , borderColor     = myppBgColor
  , fgColor         = myppTitle
  , position        = getPosition screenId pos 24
  , lowerOnStart    = True
  , overrideRedirect = True
  , hideOnStart     = False
  , allDesktops     = True
  , persistent      = True
  -- TODO: fix this
  , iconRoot        = configDir <> "/xmobar/xpm"
  , sepChar         = "%"
  , alignSep        = "}{"
  } where
    myppBgColor :: String = "#000000"
    myppTitle :: String = "#cccccc"

baseTopWidgets :: Int -> String -> [Runnable]
baseTopWidgets n pos =
  [ Run cpu
  , Run memory
  , Run diskUsage
  , Run date
  , Run $ battery ["BAT0"]
  , Run $ XPropertyLog $ screenLog n
  ]

baseBottomWidgets n pos =
  [ Run $ XPropertyLog $ screenLog n
  ] <>
  (if n == 0 then
    [Run trayer]
  else
    [Run date]
  )

getPosition :: Int -> String -> Int -> XPosition
getPosition n pos size = case pos of
  "bottom" -> OnScreen n (BottomH size)
  _        -> OnScreen n (TopH size)
