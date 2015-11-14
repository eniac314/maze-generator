import System.Random
import Control.Monad
import GHC.Word
import GHC.Int
import Data.Bits
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDLP
import qualified Graphics.UI.SDL.Image as SDLI

---------------------------------------------------------------------------------------------------
{- Graphics -}

type Point = (Double,Double)

getPixel :: Word8 -> Word8 -> Word8 -> SDL.Pixel
getPixel r g b = SDL.Pixel $ (shiftL (fi r) 24 .|. shiftL (fi g) 16 .|. shiftL (fi b) 8 .|. (fi 255)) where 
  fi a = fromIntegral a

pixelsToScreen :: [Point] -> SDL.Surface -> SDL.Pixel -> [IO Bool]
pixelsToScreen xs s p = map (\(x,y) -> SDLP.pixel s (fromIntegral.round $ x) (fromIntegral.round $ y) p) xs

linesToScreen :: [(Point,Point)] -> SDL.Surface -> SDL.Pixel -> [IO Bool]
linesToScreen xs s p = map (\((x1,y1),(x2,y2)) -> SDLP.line s (fromIntegral.round $ x1) (fromIntegral.round $ y1) (fromIntegral.round $ x2) (fromIntegral.round $ y2) p) xs


loadImage :: String -> IO SDL.Surface
loadImage filename = SDLI.load filename  >>= SDL.displayFormatAlpha

applySurface :: Int -> Int -> SDL.Surface -> SDL.Surface -> IO Bool
applySurface x y src dst = SDL.blitSurface src Nothing dst offset
    where offset = Just SDL.Rect { SDL.rectX = x, SDL.rectY = y, SDL.rectW = 0, SDL.rectH = 0 }

------------------------------------------------------------------------------------------------------
{- Main -}

screenWidth = 400
screenHeight = 400

main = SDL.withInit [SDL.InitEverything] $ do
  screen <- SDL.setVideoMode screenWidth screenHeight 32 [SDL.SWSurface]
  
  wall <- loadImage "./pics/mossy.png"
  --applySurface 0 0 wall screen
  --SDLP.polygon screen [(30,10),(10,50),(50,50)] (getPixel 255 0 0)
  SDLP.texturedPolygon screen [(30,10),(10,50),(50,50)] wall 0 0
  SDLP.texturedPolygon screen [(70,10),(50,50),(90,50)] wall 200 200


  SDL.flip screen
  SDL.delay 5000