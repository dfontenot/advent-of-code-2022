module Main where

import qualified Data.Vector as V
import Data.List
import Control.Monad.State

newtype Matrix a = Matrix (V.Vector (V.Vector a))

instance (Show a) => Show (Matrix a) where
  show (Matrix lines_) = "----\n" ++ intercalate "\n" listLines ++ "\n----"
    where
      listLines = V.toList $ V.map (\line -> intercalate " " (listLine line)) lines_
      listLine line = if (null line) then ["(empty line)"] else V.toList $ V.map show line

rotateMat :: Matrix a -> Matrix a
rotateMat (Matrix mat) = Matrix $ V.fromList $ collectMat 0 (V.length (mat V.! 0)) mat
  where
    collectMat i len mat' | i <= len - 1 = (V.map (\row -> row V.! i) mat'):(collectMat (i + 1) len mat')
    collectMat _ _ _ = []

main :: IO ()
main = do
  input <- readFile "./data/day8.txt"
  -- map (map ((read :: String -> Int) . singleton)) (lines input)
  print $ Matrix $ V.fromList $ map (\line -> V.fromList (map ((read :: String -> Int) . singleton) line)) (lines input)
