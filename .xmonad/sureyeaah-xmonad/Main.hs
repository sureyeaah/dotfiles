module Main where

import XMonad

main :: IO ()
main = xmonad $ def
         { borderWidth        = 2
         , terminal           = "kitty"
         , normalBorderColor  = "#cccccc"
         , focusedBorderColor = "#cd8b00"
         , modMask = mod4Mask              
         , focusFollowsMouse = False
         }
