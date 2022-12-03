module Main where

import qualified Data.Set as Set
import Data.List (uncons)
import Data.Char (ord, isUpper, isLower)
import Data.Maybe (fromJust)
import Data.List.Split (chunksOf)

splitHalf :: String -> Maybe (String, String)
splitHalf input = let strLen = length input in
                      let (quotient, rem') = strLen `divMod` 2 in
                          if rem' /= 0 then Nothing else Just $ splitHalf' 0 quotient strLen input
  where
    splitHalf' _ _ _ [] = ([], [])
    splitHalf' pos half len (x:xs) | pos >= half = let (left, right) = splitHalf' (pos + 1) half len xs in (left, x:right)
    splitHalf' pos half len (x:xs) = let (left, right) = splitHalf' (pos + 1) half len xs in (x:left, right)

commonItems :: String -> Maybe [Char]
commonItems str = intersection' <$> splitHalf str
  where
    intersection' (left, right) = Set.elems $ Set.fromList left `Set.intersection` Set.fromList right

itemPriority :: Char -> Int
itemPriority char | isUpper char = ord char - 38 -- ASCII codes
itemPriority char | isLower char = ord char - 96
itemPriority _ = error "bad character"

itemPriorities :: String -> Maybe [Int]
itemPriorities str = map itemPriority <$> commonItems str

part1Main :: IO ()
part1Main = do
  file <- readFile "./data/day3.txt"
  print $ sum $ map (sum . fromJust . itemPriorities) $ lines file

-- NOTE: way easier way to write this
maybeHead :: [a] -> Maybe a
maybeHead lst = onlyHead <$> uncons lst
  where
    onlyHead (head', _) = head'

threeWayIntersection :: (Ord a) => Set.Set a -> Set.Set a -> Set.Set a -> Set.Set a
threeWayIntersection a b c = (a `Set.intersection` b) `Set.intersection` c

chunkCommon :: [String] -> Maybe Char
chunkCommon chunk = chunkCommon' $ Set.fromList <$> chunk
  where
    chunkCommon' [x,y,z] = maybeHead $ Set.elems $ threeWayIntersection x y z
    chunkCommon' _ = error "chunk size does not match"

main :: IO ()
main = do
  file <- readFile "./data/day3.txt"
  print $ sum $ map (itemPriority . fromJust . chunkCommon) $ chunksOf 3 $ lines file
