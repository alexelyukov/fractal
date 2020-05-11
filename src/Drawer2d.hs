module Drawer2d (
  drawBackground,
  napkinSierpinski,
) where

import Types
import GHC.Float
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Codec.Picture(PixelRGBA8( .. ), Image)

drawBackground :: Drawing PixelRGBA8 () -> Image PixelRGBA8
drawBackground = 
  let backgroundColor = PixelRGBA8 255 255 255 255
      drawColor = PixelRGBA8 0x00 0x00 0x00 255
      width = 2000
      height = 2000
  in renderDrawing width height backgroundColor . withTexture (uniformTexture drawColor)

napkinSierpinski :: Triangle -> Int -> Drawing PixelRGBA8 ()
napkinSierpinski t n = drawTriangles $ last $ take n (split [t])

split :: [Triangle] -> [[Triangle]]
split ts =
  let toThree (Triangle (Point2d x1 y1) (Point2d x2 y2) (Point2d x3 y3)) = [Triangle (Point2d x1 y1) (Point2d ((x1 + x2) / 2) ((y1 + y2) / 2)) (Point2d ((x1 + x3) / 2) ((y1 + y3) / 2)), Triangle (Point2d ((x1 + x2) / 2) ((y1 + y2) / 2)) (Point2d x2 y2) (Point2d ((x2 + x3) / 2) ((y2 + y3) / 2)), Triangle (Point2d ((x1 + x3) / 2) ((y1 + y3) / 2)) (Point2d ((x2 + x3) / 2) ((y2 + y3) / 2)) (Point2d x3 y3)]
      result = mconcat [toThree t | t <- ts]
  in result:split result

drawTriangles :: [Triangle] -> Drawing PixelRGBA8 ()
drawTriangles = mconcat . map drawTriangle

drawTriangle :: Triangle -> Drawing PixelRGBA8 ()
drawTriangle (Triangle p1 p2 p3) = do
  drawLine (Line2d p1 p2)
  drawLine (Line2d p2 p3)
  drawLine (Line2d p3 p1)

drawLine :: Line2d -> Drawing PixelRGBA8 ()
drawLine (Line2d p0 p1) = stroke 1 JoinRound (CapRound, CapRound) $ line (preparePoint2d p0) (preparePoint2d p1)

preparePoint2d :: Point2d -> Point
preparePoint2d (Point2d x y) = V2 (double2Float x) (double2Float y)