module KochSnowflake (
  kochSnowflake,
) where

import Types
import Graphics.Rasterific
import Codec.Picture(PixelRGBA8( .. ))
import Drawer2d

kochSnowflake :: [Line2d] -> Int -> Drawing PixelRGBA8 ()
kochSnowflake ls n = drawLines $ last $ take n (split ls)

split :: [Line2d] -> [[Line2d]]
split ls =
  let toFour (Line2d (Point2d x1 y1) (Point2d x2 y2)) = [
        Line2d (Point2d x1 y1) (Point2d ((2 * x1 + x2) / 3) ((2 * y1 + y2) / 3)),
        Line2d (Point2d ((2 * x1 + x2) / 3) ((2 * y1 + y2) / 3)) (Point2d x3 y3),
        Line2d (Point2d x3 y3) (Point2d ((x1 + 2 * x2) / 3) ((y1 + 2 * y2) / 3)),
        Line2d (Point2d ((x1 + 2 * x2) / 3) ((y1 + 2 * y2) / 3)) (Point2d x2 y2)]
        where (x3, y3) = ((x1 + x2) / 2 - (y2 - y1) / (2 * sqrt 3), (x2 - x1) / (2 * sqrt 3) + (y1 + y2) / 2)
      result = mconcat [toFour l | l <- ls]
  in result:split result