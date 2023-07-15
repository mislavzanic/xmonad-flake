-- |

module Hooks.StartupHook where

import Theme.Xprop

import XMonad

import XMonad.Hooks.SetWMName

import XMonad.Util.Cursor
import XMonad.Util.SpawnOnce


trayerColor :: String -> String
trayerColor c = "--tint 0x" ++ tail c

spawnTrayer :: X()
spawnTrayer = spawn ("sleep 2 && trayer -l --edge bottom --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor primary --transparent true --alpha 0 " ++ trayerColor basebg ++ " --height 22 &")

myStartupHook :: X()
myStartupHook = do
  spawn     "killall trayer"
  -- spawn     "sleep 2" >> spawnTrayer

  spawn     ("sleep 2 && trayer -l --edge bottom --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor primary --transparent true --alpha 0 " ++ trayerColor basebg ++ " --height 22 &")
  spawnOnce "[ -f ~/.local/share/bg ] && feh --bg-fill ~/.local/share/bg"
  spawnOnce "xss-lock -- i3lock -n -i ~/.local/share/wallpaper &"
  -- spawnOnce "xss-lock -- dm-tool lock &"
  spawnOnce "emacs-28.2 --daemon &"
  spawnOnce "dunst &"
  spawnOnce "nm-applet &"
  spawnOnce "pasystray &"
  spawnOnce "xsetroot -cursor_name left_ptr &"
  spawnOnce "xmodmap ~/.config/.dotfiles/config/x11/Xmodmap"
  spawnOnce "~/.config/xmonad/xinit"

  setDefaultCursor xC_left_ptr
  setWMName "LG3D"
