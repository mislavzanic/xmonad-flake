-- |

module Hooks.HandleEventHook where

import XMonad
import XMonad.Hooks.WindowSwallowing

myHandleEventHook = swallowEventHook (className =? "Alacritty") (return True)
