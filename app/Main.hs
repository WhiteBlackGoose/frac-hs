{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where
import Data.Complex (Complex ((:+)), magnitude)
import Codec.Picture (generateImage, Pixel, PixelRGB8 (PixelRGB8), Pixel8, savePngImage, DynamicImage (ImageRGB8))
import Codec.Picture.Types (Image)
import System.IO (hSetBuffering)
import GHC.IO.StdHandles (stdin)
import GHC.IO.Handle (BufferMode(NoBuffering))

type MyReal = Double

type Criterion a b = (RealFloat a, Ord a, Pixel b) => (Complex a -> Complex a) -> Complex a -> b

belongs :: Criterion a PixelRGB8
belongs = 
  let 
    maxPrec :: Int = 50
    belongs prec crit c
      | magnitude c >= 2.0 = 
        let
          frac :: Float = fromIntegral prec / fromIntegral maxPrec
          col = 255 * frac
          col8 :: Pixel8 = round col
        in
          PixelRGB8 0 (255 - col8) (col8 `div` 2)
      | prec == 0 = PixelRGB8 255 255 255
      | otherwise = belongs (prec-1) crit (crit c)
  in
    belongs maxPrec

type FractalSeq a = (RealFloat a) => Complex a -> Complex a -> Complex a

-- https://en.wikipedia.org/wiki/Mandelbrot_set
mandelbrot :: FractalSeq a
mandelbrot c z = z ** 2 + c

-- https://en.wikipedia.org/wiki/Julia_set
julia :: FractalSeq a
julia = flip mandelbrot

render :: FractalSeq MyReal -> (Int, Int) -> (MyReal, MyReal, MyReal, MyReal) -> Image PixelRGB8
render crit (w, h) (rx, ry, rw, rh) = 
  generateImage (\x y ->
      let
        thX :: MyReal = fromIntegral x / fromIntegral w * rw + rx
        thY :: MyReal = fromIntegral y / fromIntegral h * rh + ry
        c = thX :+ thY
      in
        belongs (crit c) 0) w h


navCanvas :: Int
navCanvas = 300
qualityCanvas :: Int
qualityCanvas = 2000

interactiveMovement :: FractalSeq MyReal -> (MyReal, MyReal) -> (MyReal, MyReal) -> (Int, Int) -> IO ()
interactiveMovement set (x, y) (w, h) (cw, ch) =
  do 
    savePngImage "./out.png" (ImageRGB8 $ render set (cw, ch) (x, y, w, h))
    input <- getChar
    let zc = 1.2
    let zcc = (1 - 1/zc) / 2
    let mc = 0.1
    case input of
      '+' -> interactiveMovement set (x + w * zcc, y + h * zcc) (w/zc, h/zc) (navCanvas, navCanvas)
      '-' -> interactiveMovement set (x - w * zcc, y - h * zcc) (w*zc, h*zc) (navCanvas, navCanvas)
      'h' -> interactiveMovement set (x - w * mc, y) (w, h) (navCanvas, navCanvas)
      'j' -> interactiveMovement set (x, y + h * mc) (w, h) (navCanvas, navCanvas)
      'k' -> interactiveMovement set (x, y - h * mc) (w, h) (navCanvas, navCanvas)
      'l' -> interactiveMovement set (x + w * mc, y) (w, h) (navCanvas, navCanvas)
      'q' -> interactiveMovement set (x + w * mc, y) (w, h) (qualityCanvas, qualityCanvas)
      _ -> do
        putStrLn "Unrecognized input"
        interactiveMovement set (x, y) (w, h) (cw, ch)


main :: IO ()
main = do
  print ("Hello user!" :: String)
  print ("Use + and - to zoom" :: String)
  print ("Use hjkl to navigate" :: String)
  hSetBuffering stdin NoBuffering
  interactiveMovement julia (-1.5, -1.1) (2.2, 2.2) (navCanvas, navCanvas)
