import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)
import Vector3

type Point = Vec3 Double
type Direction = Vec3 Double
type Colour = Vec3 Int

type Ray = (Point,Direction)

rayAt :: Ray -> Double -> Point
rayAt (a,b) t = a £+ (t £* vecunit b)

putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr

drawLine :: [Colour] -> String
drawLine = concat . map drawPixel

drawPixel :: Colour -> String
drawPixel (r,g,b) = printf "%d\t%d\t%d\n" r g b

vecToColour :: Point -> Colour
vecToColour = vecmap $ floor . (*255)

rayColour :: Ray -> Colour
rayColour (a,b) = let bu   = vecunit b
                      blue = 0.5 * (vy bu + 1)
                      in vecToColour (1,1,blue)

imageWidth = 256 :: Int
imageHeight = 256 :: Int
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
                    (fromIntegral x / fromIntegral (imageWidth-1), fromIntegral y / fromIntegral (imageHeight-1), 0.25)
                    | x <- [0..imageWidth-1]
                ]
                | y <- [0..imageHeight-1]
            ]
    let colours = map (map vecToColour) image
    let lines = zip (map drawLine colours) (reverse [0..imageHeight-1])
    let monads = map (\(x,y) -> putStrLn x >> putErrLn (show y)) lines
    putStrLn $ printf "P3\n%d\t%d\t255\n" imageWidth imageHeight
    sequence_ monads