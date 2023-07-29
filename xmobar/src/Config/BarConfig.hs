{-# LANGUAGE PostfixOperators    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Config.BarConfig where

import Xmobar

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
  , position        = getPosition screenId pos
  , lowerOnStart    = True
  , overrideRedirect = True
  , hideOnStart     = False
  , allDesktops     = True
  , persistent      = True
  -- TODO: fix this
  , iconRoot        = "$XDG_CONFIG_HOME/xmobar/xpm"
  , sepChar         = "%"
  , alignSep        = "}{"
  } where
    myppBgColor :: String = "#000000"
    myppTitle :: String = "#cccccc"

getPosition :: Int -> String -> XPosition
getPosition n pos = case pos of
  "bottom" -> OnScreen n (BottomH 24)
  _        -> OnScreen n (TopH 24)
