module Fractals (
  Fractal
, mandelbrot
, julia
) where
import Data.Complex (Complex, magnitude)
import Codec.Picture (PixelRGB8(PixelRGB8))
import Codec.Picture.Types (Pixel8)

type Criterion a b = (RealFloat a, Ord a) => (Complex a -> Complex a) -> Complex a -> b

type Fractal a = Complex a -> PixelRGB8

-- https://en.wikipedia.org/wiki/Mandelbrot_set
mandelbrot :: RealFloat a => Fractal a
mandelbrot c = belongs (\z -> z ** 2 + c) 0

-- https://en.wikipedia.org/wiki/Julia_set
julia :: RealFloat a => Complex a -> Fractal a
julia p = belongs (\z -> z ** 2 + p)

belongs :: forall a. Criterion a PixelRGB8
belongs (crit :: Complex a -> Complex a) (co :: Complex a) =
  let
    maxPrec :: Int = 80
    z = takeWhile ((<=2.0) . magnitude) (iterate crit co :: [Complex a])
    n = [0..maxPrec]
    (_, iter) = last $ zip z n
  in
    color1 maxPrec iter

blend :: Float -> PixelRGB8 -> PixelRGB8 -> PixelRGB8
blend c (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) =
  let
    bl :: Pixel8 -> Pixel8 -> Pixel8
    bl a1 a2 = round $ fromIntegral a1*(1-c)+fromIntegral a2*c
  in
    PixelRGB8 (bl r1 r2) (bl g1 g2) (bl b1 b2)

color1 :: Int -> Int -> PixelRGB8
color1 maxIter iter =
  if iter == maxIter then
    PixelRGB8 0 0 0
  else
    let
      frac = 1 - fromIntegral iter / fromIntegral maxIter
      col = 255 * frac
      col8 :: Pixel8 = round col
    in
      PixelRGB8 0 (255 - col8) (col8 `div` 2)

colorEU :: Int -> Int -> PixelRGB8
colorEU maxIter iter =
  let
    frac = 1 - fromIntegral iter / fromIntegral maxIter
    euBlue = PixelRGB8 0 20 137
    euYellow = PixelRGB8 255 221 0
  in
    blend frac euYellow euBlue 

color2 :: Int -> Int -> PixelRGB8
color2 maxIter iter =
  let
    -- https://www.rapidtables.com/convert/color/hsv-to-rgb.html
    h :: Float = fromIntegral iter / fromIntegral maxIter
    s :: Float = 1.0
    v :: Float = 0.7

    c :: Float = v * s
    x :: Float = c * (fromIntegral . (1-) . abs . (`subtract` 1) . (`mod` 2) . round . (*6) $ h)
    m :: Float = v - c

    (r', g', b')
      | h <= 1/6 = (c, x, 0)
      | h <= 2/6 = (x, c, 0)
      | h <= 3/6 = (0, c, x)
      | h <= 4/6 = (0, x, c)
      | h <= 5/6 = (x, 0, c)
      | otherwise = (c, 0, x)
  in
    PixelRGB8 (round $ (r'+m)*255) (round $ (g'+m)*255) (round $ (b'+m)*255)
