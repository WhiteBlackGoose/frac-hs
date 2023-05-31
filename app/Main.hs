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
type InitialPoint a = Complex a -> Complex a

-- https://en.wikipedia.org/wiki/Mandelbrot_set
mandelbrot :: FractalSeq a
mandelbrot c z = z ** 2 + c
mandelbrotInit :: InitialPoint MyReal
mandelbrotInit _ = 0 :+ 0

-- https://en.wikipedia.org/wiki/Julia_set
julia :: Complex a -> FractalSeq a
julia c _ z =  mandelbrot c z
juliaInit :: InitialPoint MyReal
juliaInit = id

render :: FractalSeq MyReal -> InitialPoint MyReal -> (Int, Int) -> (MyReal, MyReal, MyReal, MyReal) -> Image PixelRGB8
render crit init (w, h) (rx, ry, rw, rh) =
  generateImage (\x y ->
      let
        thX :: MyReal = fromIntegral x / fromIntegral w * rw + rx
        thY :: MyReal = fromIntegral y / fromIntegral h * rh + ry
        c = thX :+ thY
      in
        belongs (crit c) (init c)) w h


navCanvas :: Int
navCanvas = 300
qualityCanvas :: Int
qualityCanvas = 2000

interactiveMovement :: FractalSeq MyReal -> InitialPoint MyReal -> (MyReal, MyReal) -> (MyReal, MyReal) -> (Int, Int) -> IO ()
interactiveMovement set init (x, y) (w, h) (cw, ch) =
  do
    savePngImage "./out.png" (ImageRGB8 $ render set init (cw, ch) (x, y, w, h))
    input <- getChar
    let zc = 1.2
    let zcc = (1 - 1/zc) / 2
    let mc = 0.1
    case input of
      '+' -> interactiveMovement set init (x + w * zcc, y + h * zcc) (w/zc, h/zc) (navCanvas, navCanvas)
      '-' -> interactiveMovement set init (x - w * zcc, y - h * zcc) (w*zc, h*zc) (navCanvas, navCanvas)
      'h' -> interactiveMovement set init (x - w * mc, y) (w, h) (navCanvas, navCanvas)
      'j' -> interactiveMovement set init (x, y + h * mc) (w, h) (navCanvas, navCanvas)
      'k' -> interactiveMovement set init (x, y - h * mc) (w, h) (navCanvas, navCanvas)
      'l' -> interactiveMovement set init (x + w * mc, y) (w, h) (navCanvas, navCanvas)
      'q' -> interactiveMovement set init (x + w * mc, y) (w, h) (qualityCanvas, qualityCanvas)
      _ -> do
        putStrLn "Unrecognized input"
        interactiveMovement set init (x, y) (w, h) (cw, ch)


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
  interactiveMovement frac juliaInit (-1.5, -1.1) (2.2, 2.2) (navCanvas, navCanvas)
