module Main where

import qualified Data.Set as Set
import Data.Char (ord, isUpper, isLower)
import Data.Maybe (fromJust)

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

itemPriorities :: String -> Maybe [Int]
itemPriorities str = map lookupPriority <$> commonItems str
  where
    lookupPriority char | isUpper char = ord char - 38 -- ASCII codes
    lookupPriority char | isLower char = ord char - 96
    lookupPriority _ = error "bad character"

main :: IO ()
main = do
  file <- readFile "./data/day3.txt"
  print $ sum $ map (sum . fromJust . itemPriorities) $ lines file
