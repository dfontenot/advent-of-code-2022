module Main where

import qualified Data.Vector as V
import Control.Concurrent.Async
import Data.List

type Coord = (Int, Int)
type GridDimens = (Int, Int)

valAtGrid :: V.Vector a -> GridDimens -> Coord -> a
valAtGrid vec (m, _) (x, y) = let pos = (m * y) + x in vec V.! pos

processCoord :: V.Vector Int -> GridDimens -> Coord -> IO Int
processCoord vec dimens startCoord = return $ upViewScore (n - 1) * downViewScore (n - 1) * leftViewScore (m - 1) * rightViewScore (m - 1)
  where
    (m, n) = dimens
    height = valAtGrid vec (m, n) startCoord
    downViewScore n' | n' == n' - 2 = if valAtGrid vec dimens (m, n') >= height then n - 1 else n
    downViewScore n' = if valAtGrid vec dimens (m, n') >= height then n - n' else downViewScore n' + 1
    upViewScore n' | n' == 0 = if valAtGrid vec dimens (m, n') >= height then n - 1 else n
    upViewScore n' = if valAtGrid vec dimens (m, n') >= height then n - n' else upViewScore n' - 1
    leftViewScore m' | m' == 0 = if valAtGrid vec dimens (m', n) >= height then m - 1 else m
    leftViewScore m' = if valAtGrid vec dimens (m', n) >= height then m - m' else leftViewScore m' - 1
    rightViewScore m' | m' == m - 2 = if valAtGrid vec dimens (m', n) >= height then m - 1 else m
    rightViewScore m' = if valAtGrid vec dimens (m', n) >= height then m' - m else rightViewScore m' + 1

main :: IO ()
main = do
  input <- readFile "./data/day8-test.txt"
  let lines' = lines input in
      let m = (length . head) lines' in
          let n = length lines' in
            let mat = V.fromList $ map ((read :: String -> Int) . singleton) $ filter (/= '\n') input in
                let allCoords = [(x, y) | x <- [1..m-2], y <- [1..n-2]] in do
                  results <- mapConcurrently (processCoord mat (m, n)) allCoords
                  print $ foldr max 0 results
                  print $ length allCoords
