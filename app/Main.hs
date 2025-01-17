module Main where

import Codec.Picture(writePng)
import Types
import Drawer2d
import NapkinSierpinski
import CarpetSierpinski
import KochSnowflake
import PythagorasTree

main :: IO ()
main = do
  -- drawNapkinSierpinski
  -- drawCarpetSierpinski
  -- drawKochSnowflake
  drawPythagorasTree

drawNapkinSierpinski :: IO ()
drawNapkinSierpinski = writePng "napkinSierpinski.png" $ drawBackground $
  napkinSierpinski (Triangle (Point2d 100 1800) (Point2d 1900 1800) (Point2d 1000 (1800 - sqrt 3 * 900))) 6

drawCarpetSierpinski :: IO ()
drawCarpetSierpinski = writePng "carpetSierpinski.png" $ drawBackground $
  carpetSierpinski (Rectangle (Point2d 100 100) (Point2d 1900 1900)) 6

drawKochSnowflake :: IO ()
drawKochSnowflake = writePng "kochSnowflake.png" $ drawBackground $
  kochSnowflake [Line2d (Point2d 400 1400) (Point2d 1600 1400),
    Line2d (Point2d 1600 1400) (Point2d 1000 (1400 - sqrt 3 * 600)),
    Line2d (Point2d 1000 (1400 - sqrt 3 * 600)) (Point2d 400 1400)] 6

drawPythagorasTree :: IO ()
drawPythagorasTree = writePng "pythagorasTree.png" $ drawBackground $
  pythagorasTree (Line2d (Point2d 1000 1700) (Point2d 1000 1400)) 12 (0.5 * pi / 4) (pi / 4) 0.83