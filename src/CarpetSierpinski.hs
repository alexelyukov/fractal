module CarpetSierpinski (
  carpetSierpinski,
) where

import Types
import Graphics.Rasterific
import Codec.Picture(PixelRGBA8( .. ))
import Drawer2d

carpetSierpinski :: Rectangle -> Int -> Drawing PixelRGBA8 ()
carpetSierpinski r n = drawRectangles $ last $ take n (split [r])

split :: [Rectangle] -> [[Rectangle]]
split rs =
  let toEight (Rectangle (Point2d x0 y0) (Point2d x1 y1)) = [
        Rectangle (Point2d x0 y0)   (Point2d x1' y1'),
        Rectangle (Point2d x1' y0)  (Point2d x2' y1'),
        Rectangle (Point2d x2' y0)  (Point2d x1 y1'),
        Rectangle (Point2d x0 y1')  (Point2d x1' y2'),
        Rectangle (Point2d x2' y1') (Point2d x1 y2'),
        Rectangle (Point2d x0 y2')  (Point2d x1' y1),
        Rectangle (Point2d x1' y2') (Point2d x2' y1),
        Rectangle (Point2d x2' y2') (Point2d x1 y1)]
        where x1' = (2 * x0 + x1) / 3
              x2' = (x0 + 2 * x1) / 3
              y1' = (2 * y0 + y1) / 3
              y2' = (y0 + 2 * y1) / 3
      result = mconcat [toEight r | r <- rs]
  in result:split result