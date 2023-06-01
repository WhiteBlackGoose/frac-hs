module SeqCheck (
  Fractal
, mandelbrot
, julia
) where
import Data.Complex (Complex)
import Codec.Picture (PixelRGB8)
import Codec.Picture.Types (Pixel8)

type Criterion a b = (RealFloat a, Ord a) => (Complex a -> Complex a) -> Complex a -> b

type Fractal a = Complex a -> PixelRGB8

belongs :: forall a. Criterion a PixelRGB8
belongs (crit :: Complex a -> Complex a) (co :: Complex a) =
  let
    maxPrec :: Int = 50
    z = takeWhile ((<=2.0) . magnitude) (iterate crit co :: [Complex a])
    n = [0..maxPrec]
    (_, iter) = last $ zip z n
  in
    if iter == maxPrec then
      PixelRGB8 0 0 0
    else
      let
        frac = 1 - fromIntegral iter / fromIntegral maxPrec
        col = 255 * frac
        col8 :: Pixel8 = round col
      in
        PixelRGB8 0 (255 - col8) (col8 `div` 2)


-- https://en.wikipedia.org/wiki/Mandelbrot_set
mandelbrot :: RealFloat a => Fractal a
mandelbrot = belongs (0, \z -> z ** 2 + c)

-- https://en.wikipedia.org/wiki/Julia_set
julia :: RealFloat a => Complex a -> Fractal a
julia p = belongs (c, snd $ mandelbrot p)

