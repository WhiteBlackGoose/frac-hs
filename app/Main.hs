{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where
import Data.Complex (Complex ((:+)), magnitude)
import Codec.Picture (generateImage, Pixel, PixelRGB8 (PixelRGB8), Pixel8, savePngImage, DynamicImage (ImageRGB8))
import Codec.Picture.Types (Image)

type Criterion a b = (RealFloat a, Ord a, Pixel b) => (Complex a -> Complex a) -> Complex a -> b

belongs :: Criterion a PixelRGB8
belongs = 
  let 
    maxPrec :: Int = 100
    belongs prec crit c
      | magnitude c >= 2.0 = 
        let
          frac :: Float = fromIntegral prec / fromIntegral maxPrec
          col = 255 * frac
          col8 :: Pixel8 = round col
        in
          PixelRGB8 0 (255 - col8 `div` 2) (col8 `div` 2)
      | prec == 0 = PixelRGB8 255 255 255
      | otherwise = belongs (prec-1) crit (crit c)
  in
    belongs maxPrec

mandelbrot :: RealFloat a => Complex a -> Complex a -> Complex a
mandelbrot c z = z ** 2 + c

render :: (Complex Float -> Complex Float -> Complex Float) -> (Int, Int) -> (Float, Float, Float, Float) -> Image PixelRGB8
render crit (w, h) (rx, ry, rw, rh) = 
  generateImage (\x y ->
      let
        thX :: Float = fromIntegral x / (fromIntegral w) * rw + rx
        thY :: Float = fromIntegral y / (fromIntegral h) * rh + ry
        c = thX :+ thY
      in
        belongs (crit c) 0) w h


main = 
    let 
      x = -1.5
      y = -1.1
      w = 2.2
      h = 2.2
      canvasW = 1000
      canvasH :: Int = 1000
    in
      savePngImage "./out.png" (ImageRGB8 $ render mandelbrot (canvasW, canvasH) (x, y, w, h))
