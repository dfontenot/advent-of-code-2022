module Main where

import qualified Data.Vector as V
import Control.Concurrent.Async
import Data.List
import GHC.Stack

type Coord = (Int, Int)
type GridDimens = (Int, Int)

valAtGrid :: HasCallStack => V.Vector a -> GridDimens -> Coord -> a
-- valAtGrid vec (m, _) (x, _) | x > m -1 = error $ "x " ++ show x ++ " is too large"
-- valAtGrid vec (_, n) (_, y) | y > n -1 = error "y is too large"
valAtGrid vec (m, _) (x, y) = let pos = (m * y) + x in vec V.! pos

processCoord :: HasCallStack => V.Vector Int -> GridDimens -> Coord -> IO Int
processCoord vec dimens (x, y) = return $ upViewScore (y - 1) * downViewScore (y + 1) * leftViewScore (x - 1) * rightViewScore (x + 1)
  where
    (m, n) = dimens
    height = valAtGrid vec (m, n) (x, y)
    valCheck :: HasCallStack => Coord -> Int
    valCheck = valAtGrid vec dimens
    downViewScore :: HasCallStack => Int -> Int
    downViewScore n' | n' == n - 1 = if valCheck (m, n') >= height then n - 1 else n
    downViewScore n' = if valCheck (m, n') >= height then n - n' else downViewScore $ n' + 1
    upViewScore :: HasCallStack => Int -> Int
    upViewScore n' | n' == 0 = if valCheck (m, n') >= height then n - 1 else n
    upViewScore n' = if valCheck (m, n') >= height then n - n' else upViewScore $ n' - 1
    leftViewScore :: HasCallStack => Int -> Int
    leftViewScore m' | m' == 0 = if valCheck (m', n) >= height then m - 1 else m
    leftViewScore m' = if valCheck (m', n) >= height then m - m' else leftViewScore $ m' - 1
    rightViewScore :: HasCallStack => Int -> Int
    rightViewScore m' | m' == m - 1 = if valCheck (m', n) >= height then m - 1 else m
    rightViewScore m' = if valCheck (m', n) >= height then m' - m else rightViewScore $ m' + 1

main :: HasCallStack => IO ()
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
