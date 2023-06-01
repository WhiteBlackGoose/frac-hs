module Colors (
  color1
, colorEU
, color2
, Color
) where

import Codec.Picture (PixelRGB8(PixelRGB8))
import Codec.Picture.Types (Pixel8)

type Color = (Int, Int) -> PixelRGB8

blend :: Float -> PixelRGB8 -> PixelRGB8 -> PixelRGB8
blend c (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) =
  let
    bl :: Pixel8 -> Pixel8 -> Pixel8
    bl a1 a2 = round $ fromIntegral a1*(1-c)+fromIntegral a2*c
  in
    PixelRGB8 (bl r1 r2) (bl g1 g2) (bl b1 b2)

color1 :: Color
color1 (maxIter, iter) =
  if iter == maxIter then
    PixelRGB8 0 0 0
  else
    let
      frac = 1 - fromIntegral iter / fromIntegral maxIter
      col = 255 * frac
      col8 :: Pixel8 = round col
    in
      PixelRGB8 0 (255 - col8) (col8 `div` 2)

colorEU :: Color
colorEU (maxIter, iter) =
  let
    frac = 1 - fromIntegral iter / fromIntegral maxIter
    euBlue = PixelRGB8 0 20 137
    euYellow = PixelRGB8 255 221 0
  in
    blend frac euYellow euBlue 

color2 :: Color
color2 (maxIter, iter) =
  let
    -- https://www.rapidtables.com/convert/color/hsv-to-rgb.html
    -- https://de.wikipedia.org/wiki/HSV-Farbraum#Umrechnung_HSV_in_RGB
    h :: Float = fromIntegral iter / fromIntegral maxIter
    hi :: Int = floor $ h * 6
    s :: Float = 1.0
    v :: Float = 0.7
    f :: Float = h * 6 - fromIntegral hi

    p :: Float = v * (1 - s)
    q :: Float = v * (1 - s * f)
    t :: Float = v * (1 - s * (1 - f))

    (r', g', b')
      | hi == 1 = (q, v, p)
      | hi == 2 = (p, v, t)
      | hi == 3 = (p, q, v)
      | hi == 4 = (t, p, v)
      | hi == 5 = (v, p, q)
      | otherwise = (v, t, p)
  in
    PixelRGB8 (round $ r'*255) (round $ g'*255) (round $ b'*255)
