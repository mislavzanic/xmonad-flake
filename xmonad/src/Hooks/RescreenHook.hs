module Hooks.RescreenHook where

import XMonad
import XMonad.Actions.Profiles

myAfterRescreenHook :: X ()
myAfterRescreenHook = spawn "[ -f ~/.local/share/wallpaper ] && feh --bg-fill ~/.local/share/wallpaper"  >> currentProfile >>= switchWSOnScreens

myRandrChangeHook :: X ()
myRandrChangeHook = spawn "autorandr --change"
