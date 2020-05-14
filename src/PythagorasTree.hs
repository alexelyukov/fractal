module PythagorasTree (
  pythagorasTree,
) where

import Types
import Graphics.Rasterific
import Codec.Picture(PixelRGBA8( .. ))
import Drawer2d
import System.Random

pythagorasTree :: Line2d -> Int -> Double -> Double -> Double -> Drawing PixelRGBA8 ()
pythagorasTree t n a b k = 
  let treeWithoutFirstStep = mconcat $ take n (splitTree [t] a b k (mkStdGen 40))
  in drawLines (t : treeWithoutFirstStep)

splitTree :: [Line2d] -> Double -> Double -> Double -> StdGen -> [[Line2d]]
splitTree ls a b k gen =
  let (_, newGen) = next gen
      toBranch (Line2d (Point2d x1 y1) (Point2d x2 y2)) = [
        Line2d (Point2d x2 y2) (Point2d (x2 - randr * len * sin ar) (y2 + randr * len * cos ar)),
        Line2d (Point2d x2 y2) (Point2d (x2 + randl * len * sin al) (y2 - randl * len * cos al))]
        where len = k * sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
              [randl, randr] = (+0.5) <$> (take 2 $ (*0.5) <$> randoms gen)
              aa = getAbsoluteAngle (Point2d x1 y1) (Point2d x2 y2)
              ar = aa - a
              al = aa + b
      result = mconcat [toBranch l | l <- ls]
  in result:splitTree result a b k newGen

getAbsoluteAngle :: Point2d -> Point2d -> Double
getAbsoluteAngle (Point2d x1 y1) (Point2d x2 y2)
  | x > 0 = atan (y / x)
  | x < 0 = atan (y / x) + pi
  | x == 0 && y > 0 = pi / 2
  | x == 0 && y < 0 = 3 * pi / 2
  where x = x2 - x1
        y = y2 - y1