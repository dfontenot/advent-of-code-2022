{-# LANGUAGE NegativeLiterals #-}
module Main where

import Matrix
import qualified Data.Vector as V
import qualified Data.Set as Set
import Data.List
import Control.Monad.State

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

treeScanFromLeftSide :: (Integral a) => Matrix a -> [TreeCoords]
treeScanFromLeftSide (Matrix forest) = forestScan 1
  where
    forestScan pos | pos == V.length forest = []
    forestScan pos | pos == V.length forest - 1 = []
    forestScan pos = numTreesInRow (forest V.! pos) pos ++ forestScan (pos + 1)

treeScanFromLeftSide1 :: (Integral a) => Matrix a -> Matrix (Maybe TreeCoords)
treeScanFromLeftSide1 forestMat = nothingMatrix `matrixUpdate` map (\coord -> (coord, Just coord)) (treeScanFromLeftSide forestMat)
  where
    nothingMatrix = Matrix $ let asVecs (Matrix m) = m in
                        let mat = asVecs forestMat in
                            let m = V.length mat in
                                let n = V.length (mat V.! 0) in
                                  V.replicate m (V.replicate n Nothing)

uniqueTreeCoords :: [Matrix (Maybe TreeCoords)] -> Set.Set TreeCoords
uniqueTreeCoords [] = Set.empty
uniqueTreeCoords (mat:rst) = foldr insertOnPresent Set.empty (mapIndexMatrix oldIdxToNew mat) `Set.union` uniqueTreeCoords rst
  where
    insertOnPresent (Just coord) set = Set.insert coord set
    insertOnPresent Nothing set = set
    oldIdxToNew (Just _) newCoords = Just newCoords
    oldIdxToNew Nothing _ = Nothing
--
-- NOTE: this is all super unoptimized and repeats a lot of scanning operations
-- I'm writing it this way to have matrix utility functions availble for other use cases
main :: IO ()
main = do
  input <- readFile "./data/day8.txt"
  -- map (map ((read :: String -> Int) . singleton)) (lines input)
  let mat = Matrix $ V.fromList $ map (V.fromList . map ((read :: String -> Int) . singleton)) (lines input) in
      let rotations = treeScanFromLeftSide1 <$> [mat, rotateMat mat, (rotateMat . rotateMat) mat, (rotateMat . rotateMat . rotateMat) mat] in -- TODO: ugly
          let unrotated = [id, rotateMat . rotateMat . rotateMat, rotateMat . rotateMat, rotateMat] <*> rotations in
              let uniqueTreeCount = Set.size $ uniqueTreeCoords unrotated in
                  print $ uniqueTreeCount + matrixPerimeter mat
          -- let numUniqueVisibleTrees = length $ Set.fromList $ concatMap treeScanFromLeftSide rotations in
          --     print $ numUniqueVisibleTrees + matrixPerimeter mat
