module Fractals (
  Fractal
, mandelbrot
, julia
) where
import Data.Complex (Complex, magnitude)

type Criterion a = (RealFloat a, Ord a) => (Complex a -> Complex a) -> Complex a -> (Int, Int)

type Fractal a = Complex a -> (Int, Int)

-- https://en.wikipedia.org/wiki/Mandelbrot_set
mandelbrot :: RealFloat a => Fractal a
mandelbrot c = belongs (\z -> z ** 2 + c) 0

-- https://en.wikipedia.org/wiki/Julia_set
julia :: RealFloat a => Complex a -> Fractal a
julia p = belongs (\z -> z ** 2 + p)

belongs :: forall a. Criterion a
belongs (crit :: Complex a -> Complex a) (co :: Complex a) =
  let
    maxPrec :: Int = 80
    z = takeWhile ((<=2.0) . magnitude) (iterate crit co :: [Complex a])
    n = [0..maxPrec]
  in
    (maxPrec, snd . last $ zip z n)
