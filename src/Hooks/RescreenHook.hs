module Hooks.RescreenHook where

import XMonad

myAfterRescreenHook :: X ()
myAfterRescreenHook = spawn "[ -f ~/.local/share/wallpaper ] && feh --bg-fill ~/.local/share/wallpaper"

myRandrChangeHook :: X ()
myRandrChangeHook = spawn "autorandr --change"
