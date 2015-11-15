module Sdl2 where
import SDL
import Linear
import Linear.Affine
import qualified Data.Text as Text
import Foreign.C.Types
import GHC.Word
import qualified Data.Vector.Storable as Vec
--import qualified SDL.Image

--------------------------------------------------------------------------
{- Loading images -}

loadOptBMP :: Window -> String -> IO Surface
loadOptBMP window path = 
  do screenSurface <- getWindowSurface window
     loadedSurface <- loadBMP path
     desiredFormat <- surfaceFormat screenSurface
     convertSurface loadedSurface desiredFormat <* freeSurface loadedSurface


--loadTexture :: Renderer -> String -> IO Texture
--loadTexture r path = 
--  do loadedSurface <- SDL.Image.load path
--     texture <- createTextureFromSurface r loadedSurface
--     freeSurface loadedSurface
--     return texture


loadBMPTexture :: Renderer -> String -> IO Texture
loadBMPTexture r path = 
  do loadedSurface <- loadBMP path
     texture <- createTextureFromSurface r loadedSurface
     freeSurface loadedSurface
     return texture

loadBMPColorKeyTexture :: Renderer -> String -> Color -> IO Texture
loadBMPColorKeyTexture renderer path (r,g,b,a) = 
  do loadedSurface <- loadBMP path
     let key = V4 r g b a
     SDL.surfaceColorKey loadedSurface $= Just key
     texture <- createTextureFromSurface renderer loadedSurface
     freeSurface loadedSurface
     return texture

--loadColorKeyTexture :: Renderer -> String -> Color -> IO Texture
--loadColorKeyTexture renderer path (r,g,b,a) = 
--  do loadedSurface <- SDL.Image.load path
--     let key = V4 r g b a
--     SDL.surfaceColorKey loadedSurface $= Just key
--     texture <- createTextureFromSurface renderer loadedSurface
--     freeSurface loadedSurface
--     return texture

renderTexture :: Renderer -> Texture -> Pnt -> IO ()
renderTexture r t (x, y) = 
  do TextureInfo _ _ w h <- queryTexture t
     copy r t Nothing (Just $ Rectangle (P $ V2 x y) (V2 w h))

renderSprite :: Renderer -> Texture -> Pnt -> Pnt -> Width -> Height -> IO ()
renderSprite r t (xs, ys) (xd, yd) w h = 
  do let source = Just $ Rectangle (P $ V2 xs ys) (V2 w h)
         destination = Just $ Rectangle (P $ V2 xd yd) (V2 w h)
     copy r t source destination 
  
--------------------------------------------------------------------------
{- Events -}

eventIsKey :: Keycode -> Event -> Bool
eventIsKey keyCode e = 
  case eventPayload e of
    KeyboardEvent keyEv ->
      keyboardEventKeyMotion keyEv == Pressed && 
        keysymKeycode (keyboardEventKeysym keyEv) == keyCode
    _                   -> False            

isEventKey :: Keycode -> Maybe Event -> Bool
isEventKey _ Nothing = False
isEventKey keyCode (Just e) = eventIsKey keyCode e

hasEventKey :: Keycode -> [Event] -> Bool
hasEventKey k =  not . null . filter (eventIsKey k)  


--------------------------------------------------------------------------
{- Graphic primitives-}

type Color = (Word8,Word8,Word8,Word8)
type Pnt = (CInt,CInt)
type Width = CInt
type Height = CInt

dFillRect :: Renderer -> Color -> Pnt -> Width -> Height -> IO ()
dFillRect renderer (r,g,b,a) (x,y) w h =
  do rendererDrawColor renderer $= V4 r g b a
     fillRect renderer (Just $ Rectangle (P $ V2 x y) (V2 w h))

dRect :: Renderer -> Color -> Pnt -> Width -> Height -> IO ()
dRect renderer (r,g,b,a) (x,y) w h =
  do rendererDrawColor renderer $= V4 r g b a
     drawRect renderer (Just $ Rectangle (P $ V2 x y) (V2 w h))

dLine :: Renderer -> Color -> Pnt -> Pnt -> IO ()
dLine renderer (r,g,b,a) (x1,y1) (x2,y2) = 
  do rendererDrawColor renderer $= V4 r g b a
     drawLine renderer (P $ V2 x1 y1) (P $ V2 x2 y2)

dPoint :: Renderer -> Color -> Pnt -> IO ()
dPoint renderer (r,g,b,a) (x,y) = 
  do rendererDrawColor renderer $= V4 r g b a
     drawPoint renderer (P $ V2 x y)

dPoints :: Renderer -> Color -> [Pnt] -> IO ()
dPoints renderer (r,g,b,a) ps = 
  let pnts = map (\(x,y) -> (P $ V2 x y)) ps in 
  do rendererDrawColor renderer $= V4 r g b a
     drawPoints renderer (Vec.fromList pnts)
--}

{-
--software rendering
main :: IO ()
main = do
  initializeAll
  window <- createWindow (Text.pack "Little Sheep 0.1") initWindow
  background <- loadOptBMP  window "../../images/background_sky.bmp"
  
  appLoop window background

  destroyWindow window
  quit 
  


appLoop :: Window -> Surface -> IO ()
appLoop w s = 
  do screenSurface <- getWindowSurface w
     surfaceBlit s Nothing screenSurface Nothing
     updateWindowSurface w
     events <- pollEvents
     unless (hasEventKey KeycodeQ events) (appLoop w s)
-}
