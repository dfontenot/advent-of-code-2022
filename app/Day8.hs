module Main where

import qualified Data.Vector as V
import Data.List

main :: IO ()
main = do
  input <- readFile "./data/day8.txt"
  print $ V.fromList $ map (map ((read :: String -> Int) . singleton)) (lines input)
