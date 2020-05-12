module Drawer2d (
  drawBackground,
  napkinSierpinski,
  carpetSierpinski,
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

carpetSierpinski :: Rectangle -> Int -> Drawing PixelRGBA8 ()
carpetSierpinski r n = drawRectangles $ last $ take n (splitCarpet [r])

napkinSierpinski :: Triangle -> Int -> Drawing PixelRGBA8 ()
napkinSierpinski t n = drawTriangles $ last $ take n (splitNapkin [t])

splitNapkin :: [Triangle] -> [[Triangle]]
splitNapkin ts =
  let toThree (Triangle (Point2d x1 y1) (Point2d x2 y2) (Point2d x3 y3)) = [
        Triangle (Point2d x1 y1) (Point2d ((x1 + x2) / 2) ((y1 + y2) / 2)) (Point2d ((x1 + x3) / 2) ((y1 + y3) / 2)),
        Triangle (Point2d ((x1 + x2) / 2) ((y1 + y2) / 2)) (Point2d x2 y2) (Point2d ((x2 + x3) / 2) ((y2 + y3) / 2)),
        Triangle (Point2d ((x1 + x3) / 2) ((y1 + y3) / 2)) (Point2d ((x2 + x3) / 2) ((y2 + y3) / 2)) (Point2d x3 y3)]
      result = mconcat [toThree t | t <- ts]
  in result:splitNapkin result

splitCarpet :: [Rectangle] -> [[Rectangle]]
splitCarpet rs =
  let toEight (Rectangle (Point2d x0 y0) (Point2d x1 y1)) = [
        Rectangle (Point2d x0 y0) (Point2d ((2 * x0 + x1) / 3) ((2 * y0 + y1) / 3)),
        Rectangle (Point2d ((2 * x0 + x1) / 3) y0) (Point2d ((x0 + 2 * x1) / 3) ((2 * y0 + y1) / 3)),
        Rectangle (Point2d ((x0 + 2 * x1) / 3) y0) (Point2d x1 ((2 * y0 + y1) / 3)),
        Rectangle (Point2d x0 ((2 * y0 + y1) / 3)) (Point2d ((2 * x0 + x1) / 3) ((y0 + 2 * y1) / 3)),
        Rectangle (Point2d ((x0 + 2 * x1) / 3) ((2 * y0 + y1) / 3)) (Point2d x1 ((y0 + 2 * y1) / 3)),
        Rectangle (Point2d x0 ((y0 + 2 * y1) / 3)) (Point2d ((2 * x0 + x1) / 3) y1),
        Rectangle (Point2d ((2 * x0 + x1) / 3) ((y0 + 2 * y1) / 3)) (Point2d ((x0 + 2 * x1) / 3) y1),
        Rectangle (Point2d ((x0 + 2 * x1) / 3) ((y0 + 2 * y1) / 3)) (Point2d x1 y1)]
      result = mconcat [toEight r | r <- rs]
  in result:splitCarpet result

drawTriangles :: [Triangle] -> Drawing PixelRGBA8 ()
drawTriangles = mconcat . map drawTriangle

drawTriangle :: Triangle -> Drawing PixelRGBA8 ()
drawTriangle (Triangle p1 p2 p3) = do
  drawLine (Line2d p1 p2)
  drawLine (Line2d p2 p3)
  drawLine (Line2d p3 p1)

drawRectangles :: [Rectangle] -> Drawing PixelRGBA8 ()
drawRectangles = mconcat . map drawRectangle

drawRectangle :: Rectangle -> Drawing PixelRGBA8 ()
drawRectangle (Rectangle (Point2d x0 y0) (Point2d x1 y1)) = do
  drawLine (Line2d (Point2d x0 y0) (Point2d x0 y1))
  drawLine (Line2d (Point2d x0 y1) (Point2d x1 y1))
  drawLine (Line2d (Point2d x1 y1) (Point2d x1 y0))
  drawLine (Line2d (Point2d x1 y0) (Point2d x0 y0))

drawLine :: Line2d -> Drawing PixelRGBA8 ()
drawLine (Line2d p0 p1) = stroke 1 JoinRound (CapRound, CapRound) $ line (preparePoint2d p0) (preparePoint2d p1)

preparePoint2d :: Point2d -> Point
preparePoint2d (Point2d x y) = V2 (double2Float x) (double2Float y)