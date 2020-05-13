module Main where

import Codec.Picture(writePng)
import Types
import Drawer2d
import NapkinSierpinski
import CarpetSierpinski

main :: IO ()
main = do
  drawNapkinSierpinski
  drawCarpetSierpinski

drawNapkinSierpinski :: IO ()
drawNapkinSierpinski = writePng "napkinSierpinski.png" $ drawBackground $
  napkinSierpinski (Triangle (Point2d 100 1800) (Point2d 1900 1800) (Point2d 1000 (1800 - sqrt 3 * 900))) 6

drawCarpetSierpinski :: IO ()
drawCarpetSierpinski = writePng "carpetSierpinski.png" $ drawBackground $
  carpetSierpinski (Rectangle (Point2d 100 100) (Point2d 1900 1900)) 6