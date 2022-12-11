{-# LANGUAGE NegativeLiterals #-}
module Main where

import qualified Data.Vector as V
import qualified Data.Set as Set
import Data.List
import Control.Monad.State

type GridDimens = (Int, Int)
type TreeCoords = (Int, Int)

numTreesInRow :: (Integral a) => V.Vector a -> Int -> [TreeCoords]
numTreesInRow row _ | V.length row == 0 = []
numTreesInRow row yCoord = evalState treeScan (0, 0, [])
  where
    treeScan = do
      (pos, heightToBeat, visibleTrees) <- get
      if pos == 0
         then put (pos + 1, row V.! pos, visibleTrees) >> treeScan
         else
          if pos == V.length row
             then return visibleTrees
             else let thisTreeHeight = row V.! pos in do
               if thisTreeHeight > heightToBeat
                  then put (pos + 1, thisTreeHeight, (pos, yCoord):visibleTrees) >> treeScan
                  else put (pos + 1, heightToBeat, visibleTrees) >> treeScan

treeScanFromLeftSide :: (Integral a) => V.Vector a -> GridDimens -> [TreeCoords]
treeScanFromLeftSide forest (m, n) = forestScan (1, 0)
  where
    forestScan (_, n') | n' == n = []
    forestScan (_, n') | n' == n - 1 = []
    forestScan (m', n') = numTreesInRow (V.slice (m' * n') m forest) n' ++ forestScan (0, n' + 1)

main :: IO ()
main = do
  input <- readFile "./data/day8.txt"
  let lines' = lines input in
      let m = (length . head) lines' in
          let n = length lines' in
            let mat = V.fromList $ map ((read :: String -> Int) . singleton) $ filter (/= '\n') input in do
                print $ treeScanFromLeftSide mat (m, n)
