module Main where

import qualified Data.Vector as V
import Data.List
import Control.Monad.State

newtype Matrix a = Matrix (V.Vector (V.Vector a))

instance (Show a) => Show (Matrix a) where
  show (Matrix lines_) = "----\n" ++ intercalate "\n" listLines ++ "\n----"
    where
      listLines = V.toList $ V.map (unwords . listLine) lines_
      listLine line = if null line then ["(empty line)"] else V.toList $ V.map show line

matrixFromList :: (Integral a) => [[a]] -> Matrix a
matrixFromList lst = Matrix $ V.fromList $ map V.fromList lst

-- turn the matrix 90 degrees counter clockwise
rotateMat :: Matrix a -> Matrix a
rotateMat (Matrix mat) = Matrix $ collectMat 0 (V.length (mat V.! 0)) mat
  where
    collectMat i len mat' | i <= len - 1 = collectMat (i + 1) len mat' V.++ V.singleton (V.map (V.! i) mat')
    collectMat _ _ _ = V.empty

numTreesInRow :: (Integral a) => V.Vector a -> Int
numTreesInRow row | V.length row == 0 = 0
numTreesInRow row = evalState treeScan (0, 0, 0)
  where
    treeScan = do
      (pos, heightToBeat, numTreesVisible) <- get
      if pos == V.length row
         then return numTreesVisible
         else let thisTreeHeight = row V.! pos in do
           if thisTreeHeight > heightToBeat
              then put (pos + 1, thisTreeHeight, numTreesVisible + 1) >> treeScan
              else put (pos + 1, heightToBeat, numTreesVisible) >> treeScan

treeScanFromLeftSide :: (Integral a) => Matrix a -> Int
treeScanFromLeftSide (Matrix forest) = forestScan 1
  where
    forestScan pos | pos == V.length forest = 0
    forestScan pos | pos == V.length forest - 1 = 0
    forestScan pos = numTreesInRow (forest V.! pos) + forestScan (pos + 1)

main :: IO ()
main = do
  input <- readFile "./data/day8.txt"
  -- map (map ((read :: String -> Int) . singleton)) (lines input)
  let mat = Matrix $ V.fromList $ map (V.fromList . map ((read :: String -> Int) . singleton)) (lines input) in
      let rotations = [mat, rotateMat mat, (rotateMat . rotateMat) mat, (rotateMat . rotateMat . rotateMat) mat] in -- TODO: ugly
          print $ sum $ map treeScanFromLeftSide rotations
