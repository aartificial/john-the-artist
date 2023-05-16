module Main where

import Artist
import UdGraphic
import Test.QuickCheck
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT hiding (Angle)

main :: IO ()
main = display (flor 20)