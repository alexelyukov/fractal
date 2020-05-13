module Drawer2d (
  drawBackground,
  drawRectangles,
  drawTriangles,
) where

import Types
import GHC.Float
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Codec.Picture(PixelRGBA8( .. ), Image)

drawBackground :: Drawing PixelRGBA8 () -> Image PixelRGBA8
drawBackground = 
  let backgroundColor = PixelRGBA8 0xFF 0xFF 0xFF 0xFF
      drawColor = PixelRGBA8 0x00 0x00 0x00 0xFF
      width = 2000
      height = 2000
  in renderDrawing width height backgroundColor . withTexture (uniformTexture drawColor)

drawTriangles :: [Triangle] -> Drawing PixelRGBA8 ()
drawTriangles = mconcat . map drawTriangle

drawTriangle :: Triangle -> Drawing PixelRGBA8 ()
drawTriangle (Triangle p1 p2 p3) = do
  let p1'= preparePoint2d p1
      p2'= preparePoint2d p2
      p3'= preparePoint2d p3
  fill [Line p1' p2', Line p2' p3', Line p3' p1']

drawRectangles :: [Rectangle] -> Drawing PixelRGBA8 ()
drawRectangles = mconcat . map drawRectangle

drawRectangle :: Rectangle -> Drawing PixelRGBA8 ()
drawRectangle (Rectangle p1 p2) = do
  let (V2 x0' y0') = preparePoint2d p1
      (V2 x1' y1') = preparePoint2d p2
  withTexture (uniformTexture (PixelRGBA8 0x00 0x00 0x00 255)) . fill $ rectangle (V2 x0' y0') (x1' - x0') (y1' - y0')

drawLine :: Line2d -> Drawing PixelRGBA8 ()
drawLine (Line2d p0 p1) = stroke 1 JoinRound (CapRound, CapRound) $ line (preparePoint2d p0) (preparePoint2d p1)

preparePoint2d :: Point2d -> Point
preparePoint2d (Point2d x y) = V2 (double2Float x) (double2Float y)