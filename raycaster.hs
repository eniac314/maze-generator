import qualified Data.Vector as Vec
import Data.Fixed (mod')
import GHC.Word
import GHC.Int
import Data.Bits
import Data.List
import SDL
import Sdl2
--import SDL.Image
import qualified Data.Text as Text
import Foreign.C.Types
import Linear hiding (angle)
import Linear.Affine
import Control.Monad
import System.Random


data World = World {
  screen :: Renderer,
  --font :: SDLT.Font,
  avatar :: Player,
  direct :: Move
}

data Player = Player {
              fov :: Double,
              unitPos :: (Double,Double),
              angle :: Double 
              } deriving Show

data Move = Forward | Backward | Left | Right | TurnLeft | TurnRight | Stop

data Wall = Wall (Double,Int,Side) | Wrong deriving Show

data Side = Hori | Verti deriving Show

distFromPlane :: Player -> Int
distFromPlane p = round $ ((fst plane) / 2) / (tan (fov p / 2))

firstHInter :: Player -> Double -> Int -> (Double,Double)
firstHInter p a o =  let yA | o == 1 = (fI.floor $ ((snd.unitPos $ p) / (gridSize))) * (gridSize) - 1.0
                            | o == (-1) = (fI.floor $ ((snd.unitPos $ p) / (gridSize))) * (gridSize) + (gridSize)
                         xA = (fst.unitPos $ p) + ((snd.unitPos $ p) - yA) / (tan a)
                     in (xA, yA)
                      


firstVInter :: Player -> Double -> Int -> (Double,Double)
firstVInter p a o = let xB | o == 1 = (fI.floor $ ((fst.unitPos $ p) / (gridSize))) * (gridSize) + (gridSize)
                           | o == (-1) = (fI.floor $ ((fst.unitPos $ p) / (gridSize))) * (gridSize) - 1.0
                        yB = (snd.unitPos $ p) + ((fst.unitPos $ p) - xB) * (tan a)
                    in (xB, yB)



findHWall :: Player -> Double -> Mat Int -> Wall
findHWall p a m = 
  let orient = isUp a
  in if orient /= 0 
     then let (xA,yA) = firstHInter p a orient
              stepY = (fI orient) * gridSize
              stepX = stepY / (tan a)
              loop 0 acc = Wrong
              loop n acc | (kindOfWall acc m == (-1)) = Wrong
                         | (kindOfWall acc m /= 0) = Wall (dist (unitPos p) acc, (kindOfWall acc m),Hori)
                         | otherwise = loop (n-1)  ((fst acc) + stepX, (snd acc) - stepY)
          in loop maxDist (xA,yA)
     else Wrong
  where dist (x,b) (c,d) = sqrt((x-c)^2 + (b-d)^2)               

findVWall :: Player -> Double -> Mat Int -> Wall
findVWall p a m = 
  let orient = isRight a
  in if orient /= 0 
     then let (xB,yB) = firstVInter p a orient
              stepX = (fI orient) * gridSize
              stepY = stepX * (tan a)
              loop 0 acc = Wrong
              loop n acc | (kindOfWall acc m == (-1)) = Wrong
                         | (kindOfWall acc m /= 0) = Wall (dist (unitPos p) acc, (kindOfWall acc m), Verti)
                         | otherwise = loop (n-1)  ((fst acc) + stepX, (snd acc) - stepY)
          in loop maxDist (xB,yB)
     else Wrong
  where dist (x,b) (c,d) = sqrt((x-c)^2 + (b-d)^2)

castRay :: Player -> Double ->  Mat Int -> Wall
castRay p a m = 
  case (findVWall p a m, findHWall p a m) of 
    (Wall(d1,t1,s1),Wrong) -> Wall (cos (betaDiff a (angle p)) * d1,t1,s1)
    (Wrong,Wall(d1,t1,s1)) -> Wall (cos (betaDiff a (angle p)) * d1,t1,s1)
    (Wall(d1,t1,s1),Wall(d2,t2,s2)) -> if (d1 <= d2)
                                       then Wall (cos (betaDiff a (angle p)) * d1,t1,s1)
                                       else Wall (cos (betaDiff a (angle p)) * d2,t2,s2)
    _ -> Wrong

fromWall (Wall w) = w
fromWall Wrong = (10000,(-1),Hori)

betaDiff a b =  min ((2 * pi) - abs(a - b)) (abs(a - b))

castRays :: Player -> Mat Int -> [Wall]
castRays p m = let angInc = (fov p) / (fst plane)
                   start1 = (-1) + div (floor.fst $ plane) 2
                   start2 = start1 + 1
      
                   loop1 0 acc inc = acc
                   loop1 n acc inc = loop1 (n-1) (castRay p inc m : acc) (inc+angInc)

                   loop2 0 acc inc = acc
                   loop2 n acc inc = loop2 (n-1) (acc++[ castRay p inc m ]) (inc-angInc)

               in (loop1 start1 [] (angle p))++(castRay p (angle p) m):(loop2 start2 [] (angle p))  


getProjections :: Player -> [Wall] -> [Wall]
getProjections p walls = let dp = fI.distFromPlane $ p 
                         in map (\w -> let (d,t,s) = fromWall w in Wall ((dp*gridSize/d),t,s)) walls
 

isUp a | sin a > 0 = 1
       | sin a < 0 = -1
       | otherwise = 0

isRight a | cos a > 0 = 1
          | cos a < 0 = -1
          | otherwise = 0

isAWall :: (Double,Double) -> Mat Int -> Bool
isAWall (x,y) m | ((floor $ y/gridSize) >= by
                   || (floor $ x/gridSize) >= bx
                   || (floor $ y/gridSize) < 0
                   || (floor $ x/gridSize) < 0) = False

                | (m § (floor $ y/gridSize, floor $ x/gridSize)) == 0 = False 
                | otherwise = True


kindOfWall :: (Double,Double) -> Mat Int -> Int
kindOfWall (x,y) m | ((floor $ y/gridSize) >= by
                     || (floor $ x/gridSize) >= bx
                     || (floor $ y/gridSize) < 0
                     || (floor $ x/gridSize) < 0) = (-1)
                   | otherwise = (m § (floor $ y/gridSize, floor $ x/gridSize))

normalize a | a > 0 = mod' a (2*pi)
            | a < 0 = (2*pi)+(mod' a ((-2)*pi))
            | otherwise = 0

fI :: (Integral a, Num b) => a -> b
fI = fromIntegral

projToLines :: [Wall] -> Renderer -> IO [()]
projToLines ws r = sequence $ map (\(w,n) -> wallToLine w r n) (zip ws [1..]) 

wallToLine :: Wall -> Renderer -> Double -> IO ()
wallToLine w r n = let (d,t,si) =  fromWall w
                       h = div (fI.snd $ plane) 2
                       hl = round (d/2)
                       n' = fI . floor $ n
                   in dLine r (wallTypeToColor (t,si)) (n',(h-hl)) (n',(h+hl))     
                      


wallTypeToColor :: (Int,Side) -> Color
wallTypeToColor (1,Hori) = getPixel 24 24 229 
wallTypeToColor (1,Verti) =  getPixel 41 41 172
wallTypeToColor (2,Hori) = getPixel 243 28 28
wallTypeToColor (2,Verti) =  getPixel 190 8 8
wallTypeToColor (3,Hori) = getPixel 11 239 92
wallTypeToColor (3,Verti) =  getPixel 24 138 64
wallTypeToColor (4,Hori) = getPixel 138 24 122
wallTypeToColor (4,Verti) =  getPixel 86 37 79
wallTypeToColor (5,Hori) = getPixel 255 165 0
wallTypeToColor (5,Verti) =  getPixel 194 130 14
wallTypeToColor ((-1),Hori) = getPixel 255 255 255
wallTypeToColor ((-1),Verti) =  getPixel 255 255 255


{-
sin = opp/hyp
cos = ad/hyp
tan = opp/adj
-}
---------------------------------------------------------------------------------------------------
{- Graphics -}

getPixel :: Word8 -> Word8 -> Word8 -> Color
getPixel r g b = (r,g,b,0)

pixelsToScreen :: [Pnt] -> Renderer -> Color -> [IO ()]
pixelsToScreen xs r c = map (\p -> dPoint r c p) xs

linesToScreen :: [(Pnt,Pnt)] -> Renderer -> Color -> [IO ()]
linesToScreen xs r c = map (\(p1,p2) -> dLine r c p1 p2) xs

--------------------------------------------------------------------------------------------------------
{- Movments -}

move :: World -> IO World
move w =
 let p = avatar w

     loop 0 w _ _ _  = return w
     loop n w sX sY sA = do let render = screen w
                                pl = avatar w
                                rays = castRays pl map1
                                proj = getProjections pl rays
                                newP = pl { unitPos = (sX + (fst.unitPos $ pl), sY + (snd.unitPos $ pl)),
                                           angle = sA + angle pl}
                            
                            start <- ticks 
                            
                            rendererDrawColor (screen w) $= V4 0 0 0 0      
                            clear render
                            projToLines proj render
                            --rendererDrawColor (screen w) $= V4 0 0 0 0 
                            present render
                            


                            stop <- ticks
                            let del = (timeDelay start stop)
                            unless (del == 0) (delay del)
                            --putStrLn (show del)
                            loop (n-1) (w {avatar = newP}) sX sY sA
    
     process n w sX sY sA = if isAWall (n*sX + (fst.unitPos $ p), n*sY + (snd.unitPos $ p)) map1
                            then return w
                            else loop n w sX sY sA 

 in case (direct w) of
     Stop -> renderWorld w
     Forward -> process 4 w (16 * (cos.angle $ p)) (-16 * (sin.angle $ p)) 0
     Backward -> process 4 w (-16 * (cos.angle $ p)) (16 * (sin.angle $ p)) 0
     Main.Left -> process 4 w (16 * (cos (angle p + pi/2))) (-16 * (sin (angle p + pi/2))) 0
     Main.Right -> process 4 w (16 * (cos (angle p - pi/2))) (-16 * (sin (angle p - pi/2))) 0
     TurnLeft -> process 6 w 0 0 (pi/24)
     TurnRight -> process 6 w 0 0 (-pi/24)

             
liftIO :: a -> IO a
liftIO x = return x

incFov :: Player -> Player
incFov p = let newFov = fov p + (5*pi/360)
           in p {fov =  newFov}                           

decFov :: Player -> Player
decFov p = let newFov = fov p - (5*pi/360)
           in p {fov =  newFov}

renderWorld :: World -> IO World
renderWorld w = do do let p = avatar w
                          r = screen w
                          rays = castRays p map1
                          proj = getProjections p rays
                      --clear r
                      rendererDrawColor (screen w) $= V4 0 0 0 0      
                      clear r
                      projToLines proj r
                      --rendererDrawColor (screen w) $= V4 0 0 0 0
                      --textToSur (toString p) (font w) 560 10 10 30 s
                      present r
                      return w

toString :: Player -> String
toString p = "Fov: " ++ ((take 7).show.toDeg.fov $ p) ++ "\n" ++
             "UnitPos: " ++ (show $ ((take 5).show.fst.unitPos $ p,(take 5).show.snd.unitPos $ p)) ++ "\n" ++
             "GridPos: " ++ (show $ ((floor $ (fst.unitPos $ p) / gridSize),(floor $ (snd.unitPos $ p) / gridSize))) ++ "\n" ++
             "Angle: " ++ ((take 7).show.toDeg.angle $ p) ++ "\n" 

toDeg :: Double -> Double
toDeg a = a * 360/(2*pi)

timeDelay start stop = let res = 30 - (stop - start)
                       in if (res > 0 && res < 100 ) then res else 0                      
-----------------------------------------------------------------------------------------------------
{- Text 

getTextSurface :: String -> SDLT.Font -> IO SDL.Surface
getTextSurface s f = SDLT.renderTextSolid f s (SDL.Color 191 191 191)

textToSur :: String -> SDLT.Font -> Int -> Int-> Int -> Int -> SDL.Surface -> IO Bool
textToSur text f x y nl nc out =
  let ts = concat $ (format nc) (lines text)
      sl = map (flip getTextSurface $ f) (drop (length ts - nl) ts)
  in do ls <- sequence sl
        foldl' (\a (s,(x,y)) -> do b1 <- a
                                   b2 <- applySurface x y s out
                                   return (b1 && b2)) (return True) (zip ls [(x,y) | y <- [y,y+20..]])

format :: Int -> [String] -> [[String]]
format  n xs = let chunk _ [] = ([],[])
                   chunk n (x:xs) | n == 0 = ([x],xs)
                               | n < 15 && x == ' ' = ([x],xs) 
                               | otherwise = let (a,b) = chunk (n-1) xs in (x:a,b)
                   
                   go cs = case (chunk n cs) of (c,[]) -> [c]
                                                (c,r)  -> [c] ++ go r

               in map go xs                                    
-}
-------------------------------------------------------------------------------------------------------
{-Main-}

screenWidth = 800
screenHeight = 600
--fontName = "DejaVuSansMono.ttf"
--fontSize = 14 :: Int

map1 = fromMat [
  [1,2,3,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  [4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,0,0,0,0,0,2,2,2,2,2,0,0,0,0,3,0,3,0,3,0,0,0,1],
  [1,0,0,0,0,0,2,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,0,0,0,0,0,2,0,0,0,2,0,0,0,0,3,0,0,0,3,0,0,0,1],
  [1,0,0,0,0,0,2,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,0,0,0,0,0,2,2,0,2,2,0,0,0,0,3,0,3,0,3,0,0,0,1],
  [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,4,4,4,4,4,4,4,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,4,0,4,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,4,0,0,0,0,5,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,4,0,4,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,4,0,4,4,4,4,4,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,4,4,4,4,4,4,4,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]] :: Mat Int

plane = (fI screenWidth,fI screenHeight)
gridSize = 64
maxDist = 100
bndY = gridSize * (fI $ (Vec.length map1))
bndX = gridSize * (fI $ (Vec.length (map1 Vec.! 0)))
by = (fI $ (Vec.length map1))
bx = (fI $ (Vec.length (map1 Vec.! 0)))

p1 = Player (pi/3) (95,95) (0)

initWindow = defaultWindow {windowInitialSize = V2 (fI screenWidth) (fI screenHeight)}

vsyncRendererConfig = 
  RendererConfig
   { SDL.rendererType = SDL.AcceleratedVSyncRenderer
   , SDL.rendererTargetTexture = False
   }


main = 
 do initializeAll

    HintRenderScaleQuality $= ScaleLinear
    renderQuality <- get HintRenderScaleQuality
    when (renderQuality /= ScaleLinear) $
     putStrLn "Warning: Linear texture filtering not enabled!"
    
    window <- createWindow (Text.pack "RayCaster 0.1") initWindow

    screen <- createRenderer window (-1) defaultRenderer
    --screen <- createRenderer window (-1) vsyncRendererConfig
    --fnt <- SDLT.openFont fontName fontSize
    
    let world = World screen p1 Stop

    loop world

    where loop w = do (quit,w1) <- whileEvents w
                      --rendererDrawColor (screen w) $= V4 0 0 0 0
                      --clear (screen w)
                      w2 <- move w1
                      --present (screen w)
                      
                      unless quit (loop w2)

    
          whileEvents w = do
             
             event      <- pollEvent
             
             case event of
              Nothing -> return (False,w)
              Just e -> 
               case eventPayload e of 
                 QuitEvent -> return (True,w)
                 KeyboardEvent (KeyboardEventData _ Released _ _) -> whileEvents (w {direct = Stop}) 
                 KeyboardEvent (KeyboardEventData _ _ _ (Keysym _ kc _)) -> 
                  case kc of KeycodeLeft -> whileEvents (w {direct = TurnLeft})
                             KeycodeRight -> whileEvents (w {direct = TurnRight})
                             KeycodeUp -> whileEvents (w {direct = Forward})
                             KeycodeDown -> whileEvents (w {direct = Backward})
                             KeycodeQ -> whileEvents (w {avatar = decFov (avatar w)})
                             KeycodeW -> whileEvents (w {avatar = incFov (avatar w)})
                             KeycodeEscape -> return (True,w)
                             KeycodeLCtrl -> whileEvents (w {direct = Main.Left})
                             KeycodeLAlt -> whileEvents (w {direct = Main.Right})
                             _ -> return (False,w)
                 _ -> return (False,w)


-------------------------------------------------------------------------------------------------------------------
{- Vectors -}

type Mat a = Vec.Vector (Vec.Vector a)
--type Mat a = Vec.Vector a

fromMat :: [[a]] ->  Mat a
fromMat xs = Vec.fromList [Vec.fromList xs' | xs' <- xs]

(§) :: Mat a -> (Int, Int) -> a
v § (r, c) = (v Vec.! r) Vec.! c

--set :: Mat a -> (Int, Int, a) -> Mat a
--vset m (r,c,v) = Vec.update m (singleton v)

--(//)  :: Mat a -> [(Int,Int,a)] -> Mat a
--v // xs = v Vec.// xs

printVec :: (Show a) => Mat a -> String
printVec v | (Vec.length $ Vec.tail v) == 0 = drop 9 $ (show $ Vec.head v)
           | otherwise = drop 9 $ (show $ Vec.head v) ++ '\n':printVec (Vec.tail v)

-------------------------------------------------------------------------------------------------------------------
{- Maze Generator-}

makeMap r c = fromMat $ [[1 | y <- [1.. c]]] ++ 
                        [1:[0 | y <- [1.. (c-2)]]++[1] | x <- [1..(r - 2)]] ++
                        [[1 | y <- [1.. c]]]
map2 = makeMap 5 100

randNum :: Int -> Int -> IO ()
randNum a b = do g <- getStdGen
                 print $ take 10 (randomRs (a, b) g)
                 return ()