module Renderer (
  render
) where
import Fractals (Fractal)
import Colors (Color)
import Codec.Picture (Image)
import Codec.Picture.Types (PixelRGB8)
import Codec.Picture (generateImage)
import Data.Complex (Complex((:+)))

render :: Fractal Double -> Color -> (Int, Int) -> (Double, Double, Double, Double) -> Image PixelRGB8
render frac color (w, h) (rx, ry, rw, rh) =
  generateImage (\x y ->
      let
        thX :: Double = fromIntegral x / fromIntegral w * rw + rx
        thY :: Double = fromIntegral y / fromIntegral h * rh + ry
        c = thX :+ thY
      in
        color (frac c)) w h
