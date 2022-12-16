module Main where

import qualified Data.Vector as V
import Control.Concurrent.Async
import Data.List
import GHC.Stack

data CoordDirectionScores = CoordDirectionScores
  { upScore :: Int,
  downScore :: Int,
  leftScore :: Int,
  rightScore :: Int
  }

instance Show CoordDirectionScores where
  show CoordDirectionScores { upScore=up, downScore=down, leftScore=left, rightScore=right } = "<up = " ++ show up ++ ", down = " ++ show down ++ ", left = " ++ show left ++ ", right = " ++ show right ++ ">"

type Coord = (Int, Int)
type GridDimens = (Int, Int)

coordToPoint :: HasCallStack => GridDimens -> Coord -> Int
coordToPoint (_, n) (_, y) | y >= n = error "no"
coordToPoint (m, _) (x, _) | x >= m = error "also no"
coordToPoint (m, _) (x, y) = (m * y) + x

valAtGridRow :: HasCallStack => V.Vector a -> GridDimens -> Coord -> a
valAtGridRow vec dimens coord = vec V.! coordToPoint dimens coord

valAtGridCol :: HasCallStack => V.Vector a -> GridDimens -> Coord -> a
valAtGridCol vec dimens (x, y) = valAtGridRow vec dimens (y, x)

-- each successive element is an increment in the x direction, until m units, then increment in the y
rowOrderVector :: HasCallStack => String -> V.Vector Int
rowOrderVector input = V.fromList $ map ((read :: String -> Int) . singleton) $ filter (/= '\n') input

toColOrderVector :: HasCallStack => V.Vector a -> GridDimens -> V.Vector a
toColOrderVector vec (m, n) = V.fromList $ colIterate (0, 0)
  where
    colIterate (x, _) | x == m = []
    colIterate (x, y) | y < n = valAtGridRow vec (m, n) (x, y):colIterate (x, y + 1)
    colIterate (x, y) | y == n = colIterate (x + 1, 0)
    colIterate (_, _) = error "iteration failure"

coordDirectionScore :: HasCallStack => CoordDirectionScores -> Int
coordDirectionScore CoordDirectionScores { upScore=up, downScore=down, leftScore=left, rightScore=right } = up * down * left * right

beforeCoord :: HasCallStack => V.Vector Int -> GridDimens -> Coord -> Int
beforeCoord vec dimens coord = process (x - 1) 1 $ V.slice (coordToPoint dimens coord - x) x vec
  where
    height = valAtGridRow vec dimens coord
    (x, _) = coord
    process x' seen vec' | x' == 0 = if vec' V.! x' >= height then seen else seen + 1
    process x' seen vec' = if vec' V.! x' >= height then seen else process (x' - 1) (seen + 1) vec'

afterCoord :: HasCallStack => V.Vector Int -> GridDimens -> Coord -> Int
afterCoord vec dimens coord = process 0 1 $ V.slice (coordToPoint dimens coord + 1) len vec
  where
    len = m - x + 1
    height = valAtGridRow vec dimens coord
    (x, _) = coord
    (m, _) = dimens
    process x' seen vec' | x' == len - 1 = if vec' V.! x' >= height then seen else seen + 1
    process x' seen vec' = if vec' V.! x' >= height then seen else process (x' + 1) (seen + 1) vec'

processCoord :: HasCallStack => V.Vector Int -> V.Vector Int -> GridDimens -> Coord -> IO CoordDirectionScores
processCoord mat colMat dimens coord = return $ CoordDirectionScores
  { upScore=beforeCoord colMat dimens coord,
  downScore=afterCoord colMat dimens coord,
  leftScore=beforeCoord mat dimens coord,
  rightScore=afterCoord mat dimens coord
  }

main :: HasCallStack => IO ()
main = do
  input <- readFile "./data/day8-test.txt"
  let lines' = lines input in
      let m = (length . head) lines' in
          let n = length lines' in
            let mat = rowOrderVector input in
                let colMat = toColOrderVector mat (m, n) in
                  let allCoords = [(x, y) | x <- [1..m-2], y <- [1..n-2]] in do
                    print allCoords
                    results <- mapConcurrently (processCoord mat colMat (m, n)) allCoords
                    print results
                    print $ foldr (max . coordDirectionScore) 0 results
                    print $ length allCoords
