module Main where

import qualified Data.Vector as V
import Control.Concurrent.Async
import Data.List

type Coord = (Int, Int)

processCoord :: V.Vector Int -> Coord -> IO Int
processCoord vec startCoord = print "hi" >> return 5

main :: IO ()
main = do
  input <- readFile "./data/day8-test.txt"
  let lines' = lines input in
      let m = (length . head) lines' in
          let n = length lines' in
            let mat = V.fromList $ map ((read :: String -> Int) . singleton) $ filter (/= '\n') input in
                let allCoords = [(x, y) | x <- [0..m-1], y <- [0..n-1]] in do
                  results <- mapConcurrently (processCoord mat) allCoords
                  print $ foldr max 0 results
                  print $ length allCoords
