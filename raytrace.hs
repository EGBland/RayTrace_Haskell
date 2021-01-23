{-# LANGUAGE FlexibleInstances #-} -- haskell throws a hissy fit when i try to make Sphere an instance of Hittable because it's a type synonym

import Data.List (minimumBy)
import Data.Maybe (isNothing, fromJust)
import Data.Ord (comparing)
import System.IO (hPutStrLn, stderr, writeFile)
import Text.Printf (printf)
import Vector3

type Radius = Double

type Point = Vec3 Double
type Direction = Vec3 Double
type Colour = Vec3 Int

type Ray = (Point,Direction)
type Sphere = (Point,Radius)

type Hit = (Point,Direction,Double)

class Hittable a where
    hits :: Ray -> a -> Maybe (Hit,Hit)
    hitsClosest :: Ray -> a -> (Double,Double) -> Maybe Hit

data HittableT = HittableSphere Sphere
doHit :: Ray -> HittableT -> (Double,Double) -> Maybe Hit
doHit ray (HittableSphere hittable) (a,b) = hitsClosest ray hittable (a,b)

type World = [HittableT]

instance Hittable Sphere where
    (p,d) `hits` (o,r)
        | disc >= 0 = let t1  = (-b - sqrt disc) / (2*a)
                          t2  = (-b + sqrt disc) / (2*a)
                          p1  = (p,d) `rayAt` t1
                          p2  = (p,d) `rayAt` t2
                          in Just ((p1,vecunit (p1 £- o),t1),(p2,vecunit (p2 £- o),t2))
        | otherwise = Nothing
        where centre = p £- o
              a      = d £. d
              b      = 2 * (centre £. d)
              c      = (centre £. centre) - (r*r)
              disc   = b*b - 4*a*c

    hitsClosest ray sphere (a,b) =
        let theHits = ray `hits` sphere
            t1      = (\(_,_,t) -> t) <$> fst <$> theHits
            t2      = (\(_,_,t) -> t) <$> snd <$> theHits
            in if Just a <= t1 && t1 <= Just b then fst <$> theHits
               else if Just a <= t2 && t2 <= Just b then snd <$> theHits
               else Nothing

rayIntersects :: World -> Ray -> Maybe Hit
rayIntersects world ray =
    let theHits = map (\hittable -> doHit ray hittable (0,1000)) world
        in 
        if all isNothing theHits then Nothing
        else (minimumBy (comparing $ fmap (\(_,_,t) -> t))) . (filter $ not . isNothing) $ theHits

rayAt :: Ray -> Double -> Point
(a,b) `rayAt` t = a £+ t £* vecunit b

hitsSphere :: Ray -> Sphere -> Maybe Point
(p,d) `hitsSphere` (o,r)
    | disc >= 0 = Just ((p,d) `rayAt` ((-b - sqrt disc) / (2*a)))
    | otherwise = Nothing
    where centre = p £- o
          a      = d £. d
          b      = 2 * (centre £. d)
          c      = (centre £. centre) - (r*r)
          disc   = b*b - 4*a*c
                               

putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr

drawLine :: [Colour] -> String
drawLine = concat . map drawPixel

drawPixel :: Colour -> String
drawPixel (r,g,b) = printf "%d\t%d\t%d\n" r g b

vecToColour :: Point -> Colour
vecToColour = vecmap $ floor . (*255)

rayColour :: Ray -> Maybe Hit -> Colour
rayColour r hit
    | isNothing hit = rayBackground r
    | otherwise     = let (point,normal,t) = fromJust hit
                          in vecToColour (0.5 £* (normal £+ (1,1,1)))

rayBackground :: Ray -> Colour
rayBackground (a,b) = let bu = vecunit b
                          t  = 0.5 * (vy bu + 1)
                          in vecToColour (((1-t) £* (1.0,251/255,119/255)) £+ (t £* (0.5,0.7,1.0)))

getImage :: Point -> [[Colour]]
getImage point = reverse
    [
        [
            let u = fromIntegral x / fromIntegral (imageWidth-1)
                v = fromIntegral y / fromIntegral (imageHeight-1)
                r = (eye, lowerLeftCorner £+ (u £* horizontal) £+ (v £* vertical) £+ eye)
                in rayColour r (rayIntersects world r)
            | x <- [0..imageWidth-1]
        ]
        | y <- [0..imageHeight-1]
    ]

imageWidth = 400 :: Int
imageHeight = 225 :: Int
aspectRatio = fromIntegral imageWidth / fromIntegral imageHeight :: Double

viewportHeight = 2.0 :: Double
viewportWidth = viewportHeight * aspectRatio :: Double
focalLength = 1 :: Double

eye = (0,0,0) :: Point
horizontal = (viewportWidth,0,0) :: Point
vertical = (0,viewportHeight,0) :: Point
lowerLeftCorner = eye £- (0.5 £* horizontal) £- (0.5 £* vertical) £- (0,0,focalLength)

world = [HittableSphere ((0,0,-1),0.5), HittableSphere ((0,-100.5,-1),100)] :: [HittableT]

main = putStrLn (printf "P3\n%d\t%d\t255\n" imageWidth imageHeight) >> (putStrLn . concat . (map drawLine) . getImage $ (0,0,-1))

--main = do
--    let centres = [let theta = fromIntegral t * pi / 16 in vecmap (*0.5) (cos theta, sin theta, -2.0) | t <- [0..31]]
--    let images = zip (map (concat . (map drawLine) . getImage) centres) [0..] :: [(String,Int)]
--    let monads = map (\(image,i) -> do
--        let txt = printf "P3\n%d\t%d\t255\n%s" imageWidth imageHeight image
--        let file = printf "frames/frame%d.ppm" i
--        writeFile file txt) images
--    sequence_ monads
    