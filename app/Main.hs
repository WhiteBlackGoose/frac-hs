{-# LANGUAGE OverloadedStrings #-}
module Main where
import Graphics.Blank                     -- import the blank canvas
import Data.Complex (Complex ((:+)), magnitude)
import Data.Vector.Unboxed.Base (Vector)
import Data.Word (Word8)
import Data.Vector.Unboxed (generate)

type Color = (Word8, Word8, Word8, Word8)

type Criterion a = (RealFloat a, Ord a) => (Complex a -> Complex a) -> Complex a -> Color

belongs :: Criterion a
belongs = 
  let 
    maxPrec = 100
    belongs prec seq c
      | magnitude c >= 2.0 = (255, (255 * prec) `div` maxPrec, 0, 128)
      | prec == 0 = (255, 0, 128, 0)
      | otherwise = belongs (prec-1) seq (seq c)
  in
    belongs maxPrec

mandelbrot :: RealFloat a => Complex a -> Complex a -> Complex a
mandelbrot c z = z ^ 2 + c

render :: (Complex Float -> Complex Float -> Complex Float) -> (Int, Int) -> (Float, Float, Float, Float) -> ImageData
render crit (w, h) (rx, ry, rw, rh) = 
  let
    -- vec :: Vector Word8 = generate (w * h * 4) (\i -> fromIntegral( i `mod` h `div` w) :: Word8)
    -- vec :: Vector Word8 = generate (w * h * 4) (\i -> if i `mod` 4 == 0 then 128 else 255)
    vec = generate (w * h * 4) (\i ->
      let
        thX :: Float = fromIntegral (i `div` 4 `div` h) / (fromIntegral w) * rw + rx
        thY :: Float = fromIntegral (i `div` 4 `mod` h) / (fromIntegral h) * rh + ry
        c = thX :+ thY
        (a, r, g, b) = belongs (crit c) 0
      in
        case i `mod` 4 of
          0 -> a
          1 -> r
          2 -> g
          3 -> b
        )
  in
    ImageData w h vec


main = blankCanvas 3000 $ \ context -> do -- start blank canvas on port 3000
  send context $ do                       -- send commands to this specific context
    putImageData ((render mandelbrot (400, 400) (-1.5, -1.1, 2.2, 2.2)), [0.0, 0.0])
