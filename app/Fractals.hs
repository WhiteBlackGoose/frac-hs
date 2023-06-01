module Fractals (
  Fractal
, mandelbrot
, julia
) where
import Data.Complex (Complex, magnitude)

type Fractal a = Complex a -> (Int, Int)

-- https://en.wikipedia.org/wiki/Mandelbrot_set
mandelbrot :: Fractal Double
mandelbrot c = belongs (\z -> z ** 2 + c) 0

-- https://en.wikipedia.org/wiki/Julia_set
julia :: Complex Double -> Fractal Double
julia p = belongs (\z -> z ** 2 + p)

belongs :: (Complex Double -> Complex Double) -> Complex Double -> (Int, Int)
belongs crit co =
    (maxPrec, length (take maxPrec z))
  where
    maxPrec :: Int = 80
    z = takeWhile ((<=2.0) . magnitude) (iterate crit co)
