-- |

{-# LANGUAGE ScopedTypeVariables #-}

module Theme.Xprop ( baseBorder
                   , basebg
                   , basefg
                   , base00
                   , base01
                   , base02
                   , base03
                   , base04
                   , base05
                   , base06
                   , base07
                   , base08
                   , base09
                   , base10
                   , base11
                   , base12
                   , base13
                   , base14
                   , base15
                   ) where

import System.IO.Unsafe ( unsafeDupablePerformIO )

import Data.Bifunctor ( bimap )
import Data.Char ( isSpace )
import Data.List ( find, dropWhileEnd, elemIndex )
import Data.Maybe ( catMaybes, fromMaybe )

import XMonad.Util.Run ( runProcessWithInput )

xProperty :: String -> IO String
xProperty k = fromMaybe "" . findValue k <$> runProcessWithInput "xrdb" ["-query"] []

findValue :: String -> String -> Maybe String
findValue k x = snd <$> find ((== k) . fst) (catMaybes $ splitAtColon <$> lines x)

splitAtColon :: String -> Maybe (String, String)
splitAtColon s = splitAtTrimming s <$> elemIndex ':' s

splitAtTrimming :: String -> Int -> (String, String)
splitAtTrimming s i = bimap trim (trim . tail) $ splitAt i s

trim, xprop :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
xprop = unsafeDupablePerformIO . xProperty

baseBorder :: String = xprop "st.border"
basebg :: String = xprop "st.background"
basefg :: String = xprop "st.foreground"
base00 :: String = xprop "st.color0"
base08 :: String = xprop "st.color8"
base01 :: String = xprop "st.color1"
base09 :: String = xprop "st.color9"
base02 :: String = xprop "st.color2"
base10 :: String = xprop "st.color10"
base03 :: String = xprop "st.color3"
base11 :: String = xprop "st.color11"
base04 :: String = xprop "st.color4"
base12 :: String = xprop "st.color12"
base05 :: String = xprop "st.color5"
base13 :: String = xprop "st.color13"
base06 :: String = xprop "st.color6"
base14 :: String = xprop "st.color14"
base07 :: String = xprop "st.color7"
base15 :: String = xprop "st.color15"
