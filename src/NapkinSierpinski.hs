module NapkinSierpinski (
  napkinSierpinski,
) where

import Types
import Graphics.Rasterific
import Codec.Picture(PixelRGBA8( .. ))
import Drawer2d

napkinSierpinski :: Triangle -> Int -> Drawing PixelRGBA8 ()
napkinSierpinski t n = drawTriangles $ last $ take n (split [t])

split :: [Triangle] -> [[Triangle]]
split ts =
  let toThree (Triangle (Point2d x1 y1) (Point2d x2 y2) (Point2d x3 y3)) = [
        Triangle (Point2d x1 y1) (Point2d ((x1 + x2) / 2) ((y1 + y2) / 2)) (Point2d ((x1 + x3) / 2) ((y1 + y3) / 2)),
        Triangle (Point2d ((x1 + x2) / 2) ((y1 + y2) / 2)) (Point2d x2 y2) (Point2d ((x2 + x3) / 2) ((y2 + y3) / 2)),
        Triangle (Point2d ((x1 + x3) / 2) ((y1 + y3) / 2)) (Point2d ((x2 + x3) / 2) ((y2 + y3) / 2)) (Point2d x3 y3)]
      result = mconcat [toThree t | t <- ts]
  in result:split result