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
  nextTreePos :: Int -> Int,
  makeCoord :: Int -> Int -> TreeCoords
  }

copyColumn :: V.Vector a -> GridDimens -> Int -> V.Vector a
copyColumn input (m, n) col = V.generate n selectCol
  where
    selectCol i = input V.! (i * m + col)

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
    nextTreePos = (+1),
    makeCoord = (,) -- TODO: bring in yCoord here, do not need to pass in to numTreesVisible
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
    nextTreePos = \x -> x - 1,
    makeCoord = (,)
  }

numTreesVisibleTop :: V.Vector Int -> Int -> [TreeCoords]
numTreesVisibleTop col _ | V.length col == 0 = []
numTreesVisibleTop col xCoord = evalState (numTreesVisible col xCoord) initialState
  where
    initialState = TreeScanState {
    pos = 0,
    heightToBeat = 0,
    visibleTrees = [],
    isBeginning = \_ pos -> pos == 0,
    isEnd = \len pos -> pos == len - 1,
    nextTreePos = (+1),
    makeCoord = \lhs rhs -> (rhs, lhs)
  }

numTreesVisibleBottom :: V.Vector Int -> Int -> [TreeCoords]
numTreesVisibleBottom col _ | V.length col == 0 = []
numTreesVisibleBottom col xCoord = evalState (numTreesVisible col xCoord) initialState
  where
    initialState = TreeScanState {
    pos = V.length col - 1,
    heightToBeat = 0,
    visibleTrees = [],
    isEnd = \_ pos -> pos == 0,
    isBeginning = \len pos -> pos == len - 1,
    nextTreePos = \x -> x - 1,
    makeCoord = \lhs rhs -> (rhs, lhs)
  }

numTreesVisible :: V.Vector Int -> Int -> State TreeScanState [TreeCoords]
numTreesVisible row _ | V.length row == 0 = return []
numTreesVisible row otherCoord = treeScan
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
                    then put TreeScanState {pos=nextTreePos pos, heightToBeat=thisTreeHeight, visibleTrees=makeCoord pos otherCoord:visibleTrees, ..} >> treeScan
                    else put TreeScanState {pos=nextTreePos pos, ..} >> treeScan

treeScanFromLeftSide :: V.Vector Int -> GridDimens -> [TreeCoords]
treeScanFromLeftSide forest (m, n) = forestScan (1, 1)
  where
    forestScan (_, n') | n' >= n - 1 = []
    forestScan (_, n') = numTreesVisibleLeft (V.slice n' m forest) n' ++ forestScan (1, n' + 1)

-- TODO: remove code duplication
treeScanFromRightSide :: V.Vector Int -> GridDimens -> [TreeCoords]
treeScanFromRightSide forest (m, n) = forestScan (1, 1)
  where
    forestScan (_, n') | n' >= n - 1 = []
    forestScan (_, n') = numTreesVisibleRight (V.slice n' m forest) n' ++ forestScan (1, n' + 1)

treeScanFromTopSide :: V.Vector Int -> GridDimens -> [TreeCoords]
treeScanFromTopSide forest (m, n) = forestScan (1, 1)
  where
    forestScan (m', _) | m' >= m - 1 = []
    forestScan (m', _) = numTreesVisibleTop (copyColumn forest (m, n) m') m' ++ forestScan (m' + 1, 1)

treeScanFromBottomSide :: V.Vector Int -> GridDimens -> [TreeCoords]
treeScanFromBottomSide forest (m, n) = forestScan (1, 1)
  where
    forestScan (m', _) | m' >= m - 1 = []
    forestScan (m', _) = numTreesVisibleBottom (copyColumn forest (m, n) m') m' ++ forestScan (m' + 1, 1)

main :: IO ()
main = do
  input <- readFile "./data/day8.txt"
  let lines' = lines input in
      let m = (length . head) lines' in
          let n = length lines' in
            let mat = V.fromList $ map ((read :: String -> Int) . singleton) $ filter (/= '\n') input in
                let sets = Set.fromList <$> [treeScanFromLeftSide mat (m, n), treeScanFromRightSide mat (m, n), treeScanFromTopSide mat (m, n), treeScanFromBottomSide mat (m, n)] in
                    let unioned = foldr Set.union Set.empty sets in
                        print $ (length unioned) + (m * 2) + (n * 2) - 4
