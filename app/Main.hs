{-# LANGUAGE OverloadedStrings #-}
module Main where
import Graphics.Blank                     -- import the blank canvas
import Data.Complex (Complex ((:+)), magnitude)
import Data.Vector.Unboxed.Base (Vector)
import Data.Word (Word8)
import Data.Vector.Unboxed (generate)

type Criterion a = (RealFloat a, Ord a) => (Complex a -> Complex a) -> Complex a -> Bool

belongs :: Criterion a
belongs = 
  let 
    belongs prec seq c
      | magnitude c >= 2.0 = False
      | prec == 0 = True
      | otherwise = belongs (prec-1) seq (seq c)
  in
    belongs 10

mandelbrot :: RealFloat a => Complex a -> Complex a -> Complex a
mandelbrot c z = z ^ 2 + c

render :: (Criterion a) -> (Int, Int) -> (Float, Float, Float, Float) -> ImageData
render crit (w, h) (rx, ry, rw, rh) = 
  let
    -- vec :: Vector Word8 = generate (w * h * 4) (\i -> fromIntegral( i `mod` h `div` w) :: Word8)
    -- vec :: Vector Word8 = generate (w * h * 4) (\i -> if i `mod` 4 == 0 then 128 else 255)
    vec = generate (w * h * 4) (\i ->
      let
        thX :: Float = fromIntegral (i `div` 4 `div` h) / (fromIntegral w) * rw + rx
        thY :: Float = fromIntegral (i `div` 4 `mod` h) / (fromIntegral h) * rh + ry
        c = thX :+ thY
      in
        if belongs (mandelbrot c) 0 then 128 else 0)
  in
    ImageData w h vec


main = blankCanvas 3000 $ \ context -> do -- start blank canvas on port 3000
  send context $ do                       -- send commands to this specific context
    putImageData ((render (\_ _ -> True) (100, 100) (-2.0, -2.0, 4.0, 4.0)), [0.0, 0.0])
