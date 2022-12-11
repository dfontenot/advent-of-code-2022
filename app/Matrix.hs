{-# LANGUAGE DeriveFunctor #-}
module Matrix (
  Matrix (Matrix),
  matrixToList,
  matrixFromList,
  matrixUpdate,
  mapIndexMatrix,
  matrixSelect,
  matrixPerimeter,
  rotateMat,
) where

import qualified Data.Vector as V
import Data.List

newtype Matrix a = Matrix (V.Vector (V.Vector a)) deriving Functor
--newtype Matrix a = Matrix a { asVecs :: V.Vector (V.Vector a) } deriving Functor -- TODO: fix?
type MatrixDimens = (Int, Int)

instance (Show a) => Show (Matrix a) where
  show (Matrix lines_) = "----\n" ++ intercalate "\n" listLines ++ "\n----"
    where
      listLines = V.toList $ V.map (unwords . listLine) lines_
      listLine line = if null line then ["(empty line)"] else V.toList $ V.map show line

-- TODO: fix, inefficient
instance Foldable Matrix where
  foldr fnc acc mat = foldr fnc acc $ concat $ matrixToList mat

matrixToList :: Matrix a -> [[a]]
matrixToList (Matrix mat) = V.toList $ V.map V.toList mat

matrixFromList :: [[a]] -> Matrix a
matrixFromList lst = Matrix $ V.fromList $ map V.fromList lst

-- TODO: super unoptimized, sort by MatrixDimens then do bulk updates when multiple updates to same row
matrixUpdate :: Matrix a -> [(MatrixDimens, a)] -> Matrix a
matrixUpdate matrix [] = matrix
matrixUpdate (Matrix mat) (((x, y), val):xs) = let row = mat V.! y in
                                            matrixUpdate (Matrix (mat V.// [(y, row V.// [(x, val)])])) xs

mapIndexVec :: (a -> Int -> b) -> V.Vector a -> V.Vector b
mapIndexVec fnc vec = V.fromList $ mapper 0 (V.toList vec)
  where
    mapper _ [] = []
    mapper pos (x:xs) = fnc x pos:mapper (pos + 1) xs

mapIndexMatrix :: (a -> MatrixDimens -> b) -> Matrix a -> Matrix b
mapIndexMatrix fnc (Matrix mat) = Matrix $ V.fromList $ mapIndex 0 $ V.toList mat
  where
    mapIndex _ [] = []
    mapIndex y (vec:vecRst) = mapIndexVec (\item x' -> fnc item (x', y)) vec:mapIndex (y + 1) vecRst

matrixSelect :: (a -> Bool) -> Matrix a -> [a]
matrixSelect fnc (Matrix mat) = concat $ V.toList $ V.map (V.toList . V.filter fnc) mat

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

