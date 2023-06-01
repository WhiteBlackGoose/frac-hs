{-# LANGUAGE TypeFamilies #-}
module Color (
PixelHSV8 (PixelHSV8)
) where
import Data.Word (Word8)
import Codec.Picture (Pixel (mixWith), PixelRGB8 (PixelRGB8))
import Codec.Picture.Types (Pixel8, PixelBaseComponent, ColorConvertible)

type Hue = Float
type Saturation = Float
type Value = Float
data PixelHSV = PixelHSV Hue Saturation Value deriving (Show, Ord, Eq)

instance ColorConvertible PixelHSV PixelRGB8 where
  promotePixel (PixelHSV h s v) =
    let 
      -- https://www.rapidtables.com/convert/color/hsv-to-rgb.html
      c = v * s
      x = c * (1 - abs $ (round $ h / 60) `mod` 2 - 1)
      m = v - c
    in
      | h < 60 = PixelRGB8 5 5 5
