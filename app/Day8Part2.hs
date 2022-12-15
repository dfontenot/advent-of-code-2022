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

valAtGridRow :: HasCallStack => V.Vector a -> GridDimens -> Coord -> a
valAtGridRow _ (_, n) (_, y) | y >= n = error "no"
valAtGridRow _ (m, _) (x, _) | x >= m = error "also no"
valAtGridRow vec (m, _) (x, y) = let pos = (m * y) + x in vec V.! pos

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

processCoord :: HasCallStack => V.Vector Int -> V.Vector Int -> GridDimens -> Coord -> IO CoordDirectionScores
processCoord vec colVec dimens (x, y) = return $ CoordDirectionScores { upScore=upViewScore (y - 1), downScore=downViewScore (y + 1), leftScore=leftViewScore (x - 1), rightScore=rightViewScore (x + 1) }
  where
    (m, n) = dimens
    height = valAtGridRow vec (m, n) (x, y)
    valCheck :: HasCallStack => Coord -> Int
    valCheck = valAtGridRow vec dimens
    -- downViewScore :: HasCallStack => Int -> Int
    -- downViewScore y' | y' > n - 1 = error "bad"
    -- downViewScore y' | y' == n - 1 = if valCheck (x, y') >= height then y' - y - 1 else n - y
    -- downViewScore y' = if valCheck (x, y') >= height then y' - y - 1 else downViewScore $ y' + 1
    -- upViewScore :: HasCallStack => Int -> Int
    -- upViewScore y' | y' == 0 = if valCheck (x, y') >= height then y - y' - 1 else y
    -- upViewScore y' = if valCheck (x, y') >= height then y - y' - 1 else upViewScore $ y' - 1
    leftViewScore :: HasCallStack => Int -> Int
    leftViewScore x' | x' == 0 = if valCheck (x', y) >= height then x - 1 else x
    leftViewScore x' = if valCheck (x', y) >= height then x - x' else leftViewScore $ x' - 1
    rightViewScore :: HasCallStack => Int -> Int
    rightViewScore x' | x' == m - 1 = if valCheck (x', y) >= height then x' - x - 1 else m - x
    rightViewScore x' = if valCheck (x', y) >= height then x' - x - 1 else rightViewScore $ x' + 1

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
