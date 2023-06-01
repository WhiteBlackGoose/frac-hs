{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where
import Data.Complex (Complex ((:+)), magnitude)
import Codec.Picture (generateImage, PixelRGB8 (PixelRGB8), Pixel8, savePngImage, DynamicImage (ImageRGB8))
import Codec.Picture.Types (Image)
import System.IO (hSetBuffering)
import GHC.IO.StdHandles (stdin)
import GHC.IO.Handle (BufferMode(NoBuffering))
import Fractals (Fractal)
import qualified Fractals (mandelbrot, julia)

type MyReal = Double

render :: Fractal MyReal -> (Int, Int) -> (MyReal, MyReal, MyReal, MyReal) -> DynamicImage
render frac (w, h) (rx, ry, rw, rh) =
  generateImage (\x y ->
      let
        thX :: MyReal = fromIntegral x / fromIntegral w * rw + rx
        thY :: MyReal = fromIntegral y / fromIntegral h * rh + ry
        c = thX :+ thY
      in
        frac c) w h

navCanvas :: Int
navCanvas = 300
qualityCanvas :: Int
qualityCanvas = 2000

data RenderInput = RenderInput {
    frac :: Fractal MyReal
    , x :: MyReal, y :: MyReal
    , w :: MyReal, h :: MyReal
    , cw :: Int, ch :: Int
  }

interactiveMovement :: RenderInput -> IO ()
interactiveMovement ri =
  do
    let RenderInput { frac, x, y, w, h, cw, ch } = ri
    savePngImage "./out.png" (ImageRGB8 $ render frac (cw, ch) (x, y, w, h))
    input <- getChar
    let zc = 1.2
    let zcc = (1 - 1/zc) / 2
    let mc = 0.1
    case input of
      '+' -> interactiveMovement $ ri { x = x + w * zcc, y = y + h * zcc, w = w/zc, h = h/zc }
      '-' -> interactiveMovement $ ri { x = x - w * zcc, y = y - h * zcc, w = w*zc, h = h*zc }
      'h' -> interactiveMovement $ ri { x = x - w * mc }
      'j' -> interactiveMovement $ ri { y = y + h * mc }
      'k' -> interactiveMovement $ ri { y = y - h * mc }
      'l' -> interactiveMovement $ ri { x = x + w * mc }
      'q' -> interactiveMovement $ ri { cw = qualityCanvas, ch = qualityCanvas }
      'u' -> interactiveMovement $ ri { cw = navCanvas, ch = navCanvas }
      _ -> do
        putStrLn "Unrecognized input"
        interactiveMovement ri


main :: IO ()
main = do
  mapM_ putStrLn ([
    "Hello user!",
    "Choose fractal",
    "m for mandelbrot",
    "j for julia set"
    ] :: [String])
  frType <- getLine
  frac <- case frType of
      "m" -> return Fractals.mandelbrot
      "j" -> do
        print ("Specify c in format Re :+ Im" :: String)
        cs <- getLine
        let c :: Complex MyReal = read cs
        return (Fractals.julia c)
      _ -> do
        print ("Unrecognized input, defaulting to mandelbrot" :: String)
        return Fractals.mandelbrot
  putStrLn ("Use + and - to zoom" :: String)
  putStrLn ("Use hjkl to navigate" :: String)
  hSetBuffering stdin NoBuffering
  interactiveMovement $ RenderInput {frac=frac, x=(-1.5), y=(-1.1), w=2.2, h=2.2, cw=navCanvas, ch=navCanvas}
