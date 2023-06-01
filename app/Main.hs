{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where
import Data.Complex (Complex ((:+)), magnitude)
import Codec.Picture (generateImage, Pixel, PixelRGB8 (PixelRGB8), Pixel8, savePngImage, DynamicImage (ImageRGB8))
import Codec.Picture.Types (Image)
import System.IO (hSetBuffering)
import GHC.IO.StdHandles (stdin)
import GHC.IO.Handle (BufferMode(NoBuffering))
import Codec.Picture.Metadata (Value(String))

type MyReal = Double

type Criterion a b = (RealFloat a, Ord a, Pixel b) => (Complex a -> Complex a) -> Complex a -> b

belongs :: forall a. Criterion a PixelRGB8
belongs (crit :: Complex a -> Complex a) (co :: Complex a) =
  let
    maxPrec :: Int = 50
    z = takeWhile ((<=2.0) . magnitude) (iterate crit co :: [Complex a])
    n = [0..maxPrec]
    iter = length $ zip z n
  in
    if iter == maxPrec+1 then
      PixelRGB8 0 0 0
    else
      let
        frac :: Float = 1 - fromIntegral iter / fromIntegral maxPrec
        col = 255 * frac
        col8 :: Pixel8 = round col
      in
        PixelRGB8 0 (255 - col8) (col8 `div` 2)

type Fractal a = (RealFloat a) => Complex a -> (Complex a, Complex a -> Complex a)

-- https://en.wikipedia.org/wiki/Mandelbrot_set
mandelbrot :: Fractal a
mandelbrot c = (0, \z -> z ** 2 + c)

-- https://en.wikipedia.org/wiki/Julia_set
julia :: Complex a -> Fractal a
julia p c = (c, snd $ mandelbrot p)

render :: Fractal MyReal -> (Int, Int) -> (MyReal, MyReal, MyReal, MyReal) -> Image PixelRGB8
render frac (w, h) (rx, ry, rw, rh) =
  generateImage (\x y ->
      let
        thX :: MyReal = fromIntegral x / fromIntegral w * rw + rx
        thY :: MyReal = fromIntegral y / fromIntegral h * rh + ry
        c = thX :+ thY
        (init, crit) = frac c
      in
        belongs crit init) w h


navCanvas :: Int
navCanvas = 300
qualityCanvas :: Int
qualityCanvas = 2000

interactiveMovement :: Fractal MyReal -> (MyReal, MyReal) -> (MyReal, MyReal) -> (Int, Int) -> IO ()
interactiveMovement frac (x, y) (w, h) (cw, ch) =
  do
    savePngImage "./out.png" (ImageRGB8 $ render frac (cw, ch) (x, y, w, h))
    input <- getChar
    let zc = 1.2
    let zcc = (1 - 1/zc) / 2
    let mc = 0.1
    case input of
      '+' -> interactiveMovement frac (x + w * zcc, y + h * zcc) (w/zc, h/zc) (navCanvas, navCanvas)
      '-' -> interactiveMovement frac (x - w * zcc, y - h * zcc) (w*zc, h*zc) (navCanvas, navCanvas)
      'h' -> interactiveMovement frac (x - w * mc, y) (w, h) (navCanvas, navCanvas)
      'j' -> interactiveMovement frac (x, y + h * mc) (w, h) (navCanvas, navCanvas)
      'k' -> interactiveMovement frac (x, y - h * mc) (w, h) (navCanvas, navCanvas)
      'l' -> interactiveMovement frac (x + w * mc, y) (w, h) (navCanvas, navCanvas)
      'q' -> interactiveMovement frac (x + w * mc, y) (w, h) (qualityCanvas, qualityCanvas)
      _ -> do
        putStrLn "Unrecognized input"
        interactiveMovement frac (x, y) (w, h) (cw, ch)


main :: IO ()
main = do
  mapM_ print ([
    "Hello user!",
    "Choose fractal",
    "m for mandelbrot",
    "j for julia set"
    ] :: [String])
  frType <- getLine
  frac <- case frType of
      "m" -> return mandelbrot
      "j" -> do
        print ("Specify c" :: String)
        cs <- getLine
        let c :: Complex MyReal = read cs
        return (julia c)
      _ -> do
        print ("Unrecognized input, defaulting to mandelbrot" :: String)
        return mandelbrot
  print ("Use + and - to zoom" :: String)
  print ("Use hjkl to navigate" :: String)
  hSetBuffering stdin NoBuffering
  interactiveMovement frac (-1.5, -1.1) (2.2, 2.2) (navCanvas, navCanvas)
