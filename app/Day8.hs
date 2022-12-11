{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Data.Vector as V
import qualified Data.Set as Set
import Data.List
import Control.Monad.State

type GridDimens = (Int, Int)
type TreeCoords = (Int, Int)
data TreeScanState = TreeScanState
  { pos :: Int,
  heightToBeat :: Int,
  visibleTrees :: [TreeCoords],
  isBeginning :: Int -> Int -> Bool,
  isEnd :: Int -> Int -> Bool,
  nextTreePos :: Int -> Int
  }

numTreesVisibleLeft :: V.Vector Int -> Int -> [TreeCoords]
numTreesVisibleLeft row _ | V.length row == 0 = []
numTreesVisibleLeft row yCoord = evalState (numTreesVisible row yCoord) initialState
  where
    initialState = TreeScanState {
    pos = 0,
    heightToBeat = 0,
    visibleTrees = [],
    isBeginning = \_ pos -> pos == 0,
    isEnd = \len pos -> pos == len - 1,
    nextTreePos = (+1)
  }

numTreesVisibleRight :: V.Vector Int -> Int -> [TreeCoords]
numTreesVisibleRight row _ | V.length row == 0 = []
numTreesVisibleRight row yCoord = evalState (numTreesVisible row yCoord) initialState
  where
    initialState = TreeScanState {
    pos = V.length row - 1, -- TODO: rewrite so that isEnd and isBeginning don't take length, since length can just defined here
    heightToBeat = 0,
    visibleTrees = [],
    isEnd = \_ pos -> pos == 0,
    isBeginning = \len pos -> pos == len - 1,
    nextTreePos = \x -> x - 1
  }

numTreesVisible :: V.Vector Int -> Int -> State TreeScanState [TreeCoords]
numTreesVisible row _ | V.length row == 0 = return []
numTreesVisible row yCoord = treeScan
  where
    treeScan = let len = V.length row in do
        TreeScanState {..} <- get
        if isBeginning len pos
           then put TreeScanState {pos=nextTreePos pos, heightToBeat=row V.! pos, ..} >> treeScan
           else
            if isEnd len pos
               then return visibleTrees
               else let thisTreeHeight = row V.! pos in do
                 if thisTreeHeight > heightToBeat
                    then put TreeScanState {pos=nextTreePos pos, heightToBeat=thisTreeHeight, visibleTrees=(pos, yCoord):visibleTrees, ..} >> treeScan
                    else put TreeScanState {pos=nextTreePos pos, ..} >> treeScan

treeScanFromLeftSide :: V.Vector Int -> GridDimens -> [TreeCoords]
treeScanFromLeftSide forest (m, n) = forestScan (1, 0)
  where
    forestScan (_, n') | n' == n = []
    forestScan (_, n') | n' == n - 1 = []
    forestScan (m', n') = numTreesVisibleLeft (V.slice (m' * n') m forest) n' ++ forestScan (0, n' + 1)

treeScanFromRightSide :: V.Vector Int -> GridDimens -> [TreeCoords]
treeScanFromRightSide forest (m, n) = forestScan (1, 0)
  where
    forestScan (_, n') | n' == n = []
    forestScan (_, n') | n' == n - 1 = []
    forestScan (m', n') = numTreesVisibleRight (V.slice (m' * n') m forest) n' ++ forestScan (0, n' + 1)

main :: IO ()
main = do
  input <- readFile "./data/day8.txt"
  let lines' = lines input in
      let m = (length . head) lines' in
          let n = length lines' in
            let mat = V.fromList $ map ((read :: String -> Int) . singleton) $ filter (/= '\n') input in do
                print $ treeScanFromLeftSide mat (m, n)
                print $ treeScanFromRightSide mat (m, n)
