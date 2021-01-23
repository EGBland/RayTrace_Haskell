import Data.Maybe (isNothing, fromJust)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)
import Vector3

type Radius = Double

type Point = Vec3 Double
type Direction = Vec3 Double
type Colour = Vec3 Int

type Ray = (Point,Direction)
type Sphere = (Point,Radius)

rayAt :: Ray -> Double -> Point
(a,b) `rayAt` t = a £+ (t £* vecunit b)

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

rayColour :: Ray -> Colour
rayColour r
    | isNothing pt = rayBackground r
    | otherwise = let ptj  = fromJust pt
                      ptju = vecunit (ptj £- (0,0,-1))
                      in vecToColour (vecmap ((*0.5) . (+1)) ptju) 
    where pt = r `hitsSphere` ((0,0,-1),0.5)

rayBackground :: Ray -> Colour
rayBackground (a,b) = let bu = vecunit b
                          t  = 0.5 * (vy bu + 1)
                          in vecToColour (((1-t) £* (1.0,1.0,1.0)) £+ (t £* (0.5,0.7,1.0)))

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

main = do
    let image = reverse
            [
                [
                    let u = fromIntegral x / fromIntegral (imageWidth-1)
                        v = fromIntegral y / fromIntegral (imageHeight-1)
                        in rayColour (eye, lowerLeftCorner £+ (u £* horizontal) £+ (v £* vertical) £+ eye)
                    | x <- [0..imageWidth-1]
                ]
                | y <- [0..imageHeight-1]
            ]
    let lines = zip (map drawLine image) (reverse [0..imageHeight-1])
    let monads = map (\(x,y) -> putStrLn x >> putErrLn (show y)) lines
    putStrLn $ printf "P3\n%d\t%d\t255\n" imageWidth imageHeight
    sequence_ monads