module Main where

import Data.List
import Graphics.Gloss
import System.Random.MWC

type Size = (Int, Int) -- width, height
type Schale = [[Bool]]

initialSchale :: Size -> IO Schale
initialSchale (width, height) = do
  withSystemRandom . asGenST $ \gen -> do
    sequence $ replicate height $ do
      sequence $ replicate width $ uniform gen

displaySchale :: Schale -> Picture
displaySchale schale =
  let (width, height) = (genericLength . head $ schale, genericLength schale)
      indexed = map (zip [0.. ] . sequence) . zip [0..]
      cell = rectangleSolid 10 10
      draw (x, (y, s)) = color (if s then white else black) . translate ((x-width/2)*10+5) ((y-height/2)*10+5)
    in pictures . map (pictures . map (\s -> draw s cell)) . indexed $ schale

nbhd :: Size -> (Int, Int) -> [(Int, Int)]
nbhd (w, h) (x, y) = [((w+x+dx)`mod`w, (h+y+dy)`mod`h) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]

rule :: Bool -> [Bool] -> Bool
rule False xs = length (filter id xs) == 3
rule True xs
  | length (filter id xs) <= 1 = False
  | length (filter id xs) <= 3 = True
rule _ _ = False

at :: Schale -> (Int, Int) -> Bool
at schale (x, y) = (schale!!y)!!x

nextStep :: Schale -> Schale
nextStep schale =
  let (width, height) = (length . head $ schale, length schale)
      indexed = map (zip [0.. ] . sequence) . zip [0..]
      judge (x, (y, s)) = rule s . map (schale `at`) $ nbhd (width, height) (x, y)
   in map (map judge) . indexed $ schale

main :: IO ()
main = do
  schale <- initialSchale (64,48)
  simulate (InWindow "Conway's Game of Life" (640, 480) (100, 100)) white 10 schale displaySchale (\_ _ -> nextStep)
