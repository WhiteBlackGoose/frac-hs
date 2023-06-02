{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE BangPatterns #-}
module Main where
import Data.Complex (Complex ((:+)))
import Codec.Picture (generateImage, PixelRGB8, savePngImage, DynamicImage (ImageRGB8), writeGifAnimation, GifLooping (LoopingForever))
import Codec.Picture.Types (Image)
import System.IO (hSetBuffering)
import GHC.IO.StdHandles (stdin)
import GHC.IO.Handle (BufferMode(NoBuffering))
import Fractals (Fractal)
import qualified Fractals (mandelbrot, julia)
import qualified Colors (color1, colorEU, color2)
import Colors (Color)
import Renderer (render)
import Data.Traversable (forM)

navCanvas :: Int
navCanvas = 200
qualityCanvas :: Int
qualityCanvas = 4096

data RenderInput = RenderInput {
    frac :: !(Fractal Double)
    , x,y,w,h :: !Double
    , csize :: !Int
    , color :: !Color
  }

interactiveMovement :: RenderInput -> IO ()
interactiveMovement ri =
  do
    let RenderInput { frac, x, y, w, h, csize, color } = ri
    savePngImage "./out.png" (ImageRGB8 $ render frac color 
      (csize, round $ fromIntegral csize * h / w) 
      (x, y, w, h))
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
      'q' -> interactiveMovement $ ri { csize = qualityCanvas }
      'u' -> interactiveMovement $ ri { csize = navCanvas }
      _ -> do
        putStrLn "Unrecognized input"
        interactiveMovement ri


interactIO :: IO ()
interactIO = do
  mapM_ putStrLn ([
    "Hello user!",
    "Choose fractal",
    "m for mandelbrot",
    "j for julia set"
    ] :: [String])
  let
    askForInput = do
      frType <- getLine
      case frType of
          "j" -> return (Fractals.julia ((-0.5251993) :+ (-0.5251993)), -1.5, -1, 3, 2)
          "m" -> return (Fractals.mandelbrot, -1.5, -1.1, 2.2, 2.2)
          _ -> do
            putStrLn "Unrecognized input, try again"
            askForInput
  (frac, x, y, w, h) <- askForInput
  putStrLn ("Use + and - to zoom" :: String)
  putStrLn ("Use hjkl to navigate" :: String)
  hSetBuffering stdin NoBuffering
  interactiveMovement $ RenderInput {frac=frac, x, y, w, h, csize=navCanvas, color=Colors.color2}

renderGifIO :: IO ()
renderGifIO =
  let
    e = exp 1
    juliaParams =
      map ((e**) . (0 :+)) [3.365, 3.3653..3.43]
    julias
      = map Fractals.julia juliaParams
  in do
    seq <- forM (zip julias [1..]) (\(julia, i) -> do
        putStrLn ( "Step #"++show i++"/"++show(length juliaParams) )
        -- Non-lazy evaluation to ensure correctness of printing step thingy
        let !im = render julia Colors.color2 (600, 400) (-1.5, -1, 3, 2)
        return im
      )
    let
      anim = writeGifAnimation "./out.gif" 0 LoopingForever seq
    case anim of
      Left s -> do
        putStrLn s
        return ()
      Right io ->
        io


main :: IO ()
main = renderGifIO
