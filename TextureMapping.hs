module TextureMapping where
import Codec.Picture
import Sdl2

getPixel :: DynamicImage -> (Int,Int) -> Maybe Color
getPixel (ImageRGB8 image) (x,y) = 
 let PixelRGB8 r g b = pixelAt image x y
 in Just (r,g,b,255)
getPixel _ _ = Nothing

