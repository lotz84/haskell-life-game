module Main where

import Data.List
import Graphics.Gloss
import System.Random.MWC

type Position = (Int, Int)
type Positions = [Position]
type Size = (Int, Int)
type Schale = [[Float]]

cellSize :: Float
cellSize = 10

size :: Num a => Schale -> (a, a)
size schale = (genericLength . head $ schale, genericLength schale)

at :: Schale -> (Int, Int) -> Float
at schale (x, y) = (schale!!y)!!x

initialSchale :: Size -> IO Schale
initialSchale (width, height) = do
  withSystemRandom . asGenST $ \gen -> do
    sequence $ replicate height $ do
      sequence $ replicate width $ uniformR (0, 1) gen

displaySchale :: Schale -> Picture
displaySchale schale =
  let (width, height) = size schale
      indexed = map (zip [0.. ] . sequence) . zip [0..]
      cell = rectangleSolid cellSize cellSize
      draw (x, (y, s)) = color (greyN s) . translate ((x-width/2)*cellSize+cellSize/2) ((y-height/2)*cellSize+cellSize/2)
   in pictures . map (pictures . map (\s -> draw s cell)) . indexed $ schale

outerAntiAliasing :: Float -> Float -> Float -> Float
outerAntiAliasing l ri ra
  | l < ri - 0.5 = 0
  | l < ri + 0.5 = l - ri + 0.5
  | ra + 0.5 < l = 0
  | ra - 0.5 < l = ra + 0.5 - l
  | otherwise =  1

outerIntegral :: Int -> Int -> Schale -> Position -> Float
outerIntegral ri ra schale (x, y) =
  let (w, h) = size schale
      area = [((w+x+u)`mod`w, (h+y+v)`mod`h, sqrt ((fromIntegral u)^2+(fromIntegral v)^2)) | u <- [-ra..ra], v <- [-ra..ra], ri^2 <= u^2+v^2]
   in (sum $ map (\(xu, yv, d) -> outerAntiAliasing d (fromIntegral ri) (fromIntegral ra) * schale `at` (xu, yv)) area) / genericLength area

innerAntiAliasing :: Float -> Float -> Float
innerAntiAliasing l ri
  | l < ri - 0.5 = 1
  | ri + 0.5 < l = 0
  | otherwise =  ri + 0.5 - l

innerIntegral :: Int -> Schale -> Position -> Float
innerIntegral ri schale (x, y) =
  let (w, h) = size schale
      area = [((w+x+u)`mod`w, (h+y+v)`mod`h, sqrt ((fromIntegral u)^2+(fromIntegral v)^2)) | u <- [-ri..ri], v <- [-ri..ri]]
   in (sum $ map (\(xu, yv, d) -> innerAntiAliasing d (fromIntegral ri) * schale `at` (xu, yv)) area) / genericLength area

sigma_1 :: Float -> Float -> Float -> Float
sigma_1 alpha x a = 1 / (1 + exp ((-4) * (x - a) / alpha))

sigma_2 :: Float -> Float -> Float -> Float -> Float
sigma_2 alpha x a b = sigma_1 alpha x a * (1 - sigma_1 alpha x b)

sigma_m :: Float -> Float -> Float -> Float -> Float
sigma_m alpha x y m = x * (1 - sigma_1 alpha m 0.5) + y * sigma_1 alpha m 0.5

snm :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float
snm alpha_n alpha_m n m b1 b2 d1 d2 = sigma_2 alpha_n n (sigma_m alpha_m b1 d1 m) (sigma_m alpha_m b1 d1 m)

nextStep :: Schale -> Schale
nextStep schale =
  let (width, height) = size schale
      indexed = map (zip [0.. ] . sequence) . zip [0..]
      life (x, (y, s)) = snm 0.028 0.147 (innerIntegral 7 schale (x, y)) (outerIntegral 7 21 schale (x, y)) 0.278 0.365 0.267 0.445
   in map (map life) . indexed $ schale

main :: IO ()
main = do
  schale <- initialSchale (64,48)
  simulate (InWindow "Conway's Game of Life" (640, 480) (100, 100)) white 1 schale displaySchale (\_ _ -> nextStep)
