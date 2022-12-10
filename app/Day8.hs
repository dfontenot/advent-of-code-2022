{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE DeriveFunctor #-}
module Main where

import qualified Data.Vector as V
import qualified Data.Set as Set
import Data.List
import Control.Monad.State

newtype Matrix a = Matrix (V.Vector (V.Vector a)) deriving Functor
type TreeCoords = (Int, Int)
type MatrixDimens = (Int, Int)

instance (Show a) => Show (Matrix a) where
  show (Matrix lines_) = "----\n" ++ intercalate "\n" listLines ++ "\n----"
    where
      listLines = V.toList $ V.map (unwords . listLine) lines_
      listLine line = if null line then ["(empty line)"] else V.toList $ V.map show line

matrixFromList :: (Integral a) => [[a]] -> Matrix a
matrixFromList lst = Matrix $ V.fromList $ map V.fromList lst

mapIndexVec :: (a -> Int -> b) -> V.Vector a -> V.Vector b
mapIndexVec fnc vec = V.fromList $ mapper 0 (V.toList vec)
  where
    mapper _ [] = []
    mapper pos (x:xs) = fnc x pos:mapper (pos + 1) xs

indexMapMatrix :: (a -> TreeCoords -> b) -> Matrix a -> Matrix b
indexMapMatrix fnc (Matrix mat) = Matrix $ V.fromList $ mapIndex 0 $ V.toList mat
  where
    mapIndex _ [] = []
    mapIndex y (vec:vecRst) = mapIndexVec (\item x' -> fnc item (x', y)) vec:mapIndex (y + 1) vecRst

-- NOTE: assumes non-jagged matrix
matrixPerimeter :: Matrix a -> Int
matrixPerimeter (Matrix mat) = let m = V.length mat in
                                   let n = V.length (mat V.! 0) in m * 2 + n * 2 - 4

-- turn the matrix 90 degrees counter clockwise
rotateMat :: Matrix a -> Matrix a
rotateMat (Matrix mat) = Matrix $ collectMat 0 (V.length (mat V.! 0)) mat
  where
    collectMat i len mat' | i <= len - 1 = collectMat (i + 1) len mat' V.++ V.singleton (V.map (V.! i) mat')
    collectMat _ _ _ = V.empty

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

main :: IO ()
main = do
  input <- readFile "./data/day8.txt"
  -- map (map ((read :: String -> Int) . singleton)) (lines input)
  let mat = Matrix $ V.fromList $ map (V.fromList . map ((read :: String -> Int) . singleton)) (lines input) in
      let rotations = [mat, rotateMat mat, (rotateMat . rotateMat) mat, (rotateMat . rotateMat . rotateMat) mat] in -- TODO: ugly
          let numUniqueVisibleTrees = length $ Set.fromList $ concatMap treeScanFromLeftSide rotations in
              print $ numUniqueVisibleTrees + matrixPerimeter mat
