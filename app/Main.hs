{-# LANGUAGE OverloadedStrings #-}
module Main where
import Graphics.Blank                     -- import the blank canvas
import Data.Complex (Complex, magnitude)
import Data.Vector.Unboxed.Base (Vector)
import Data.ByteArray (ByteArray)
import Data.Word (Word8)
import Data.Vector.Unboxed (generate)

type Criterion a = (RealFloat a, Ord a) => (Complex a -> Complex a) -> Complex a -> Bool

belongs :: Criterion a
belongs = 
  let 
    belongs prec seq c
      | magnitude c >= 2.0 = False
      | prec == 1 = True
      | otherwise = belongs (prec-1) seq (seq c)
  in
    belongs 10


render :: (Criterion a) -> (Int, Int) -> ImageData
render crit (w, h) = 
  let
    vec :: Data.Vector.Unboxed.Base.Vector Word8 = generate (w * h) (\i -> fromIntegral( i `mod` h `div` w) :: Word8)
  in
    ImageData w h vec


main = blankCanvas 3000 $ \ context -> do -- start blank canvas on port 3000
  send context $ do                       -- send commands to this specific context
    moveTo(50,50)
    lineTo(200,100)
    lineWidth 10
    strokeStyle "red"
    stroke()                              -- this draws the ink into the canvas
