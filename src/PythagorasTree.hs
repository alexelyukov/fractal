module PythagorasTree (
  pythagorasTree,
) where

import Types
import Graphics.Rasterific
import Codec.Picture(PixelRGBA8( .. ))
import Drawer2d

pythagorasTree :: Line2d -> Int -> Double -> Double -> Double -> Drawing PixelRGBA8 ()
pythagorasTree t n a b k = 
  let treeWithoutFirstStep = mconcat $ take n (split [t] a b k)
  in drawLines (t : treeWithoutFirstStep)

split :: [Line2d] -> Double -> Double -> Double -> [[Line2d]]
split ls a b k =
  let toBranch (Line2d (Point2d x1 y1) (Point2d x2 y2)) = [
        Line2d (Point2d x2 y2) (Point2d (x2 + len * sin ar) (y2 + len * cos ar)),
        Line2d (Point2d x2 y2) (Point2d (x2 + len * sin al) (y2 + len * cos al))]
        where len = k * sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
              aa = getAbsoluteAngle (Point2d x1 y1) (Point2d x2 y2)
              ar = aa - a
              al = aa + b
      result = mconcat [toBranch l | l <- ls]
  in result:split result a b k

getAbsoluteAngle :: Point2d -> Point2d -> Double
getAbsoluteAngle (Point2d x1 y1) (Point2d x2 y2)
  | x > 0 = atan (y / x)
  | x < 0 = atan (y / x) + pi
  | x == 0 && y > 0 = pi / 2
  | x == 0 && y < 0 = 3 * pi / 2
  where x = x2 - x1
        y = y2 - y1