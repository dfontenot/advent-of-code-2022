module Main where

import Data.Set

splitHalf :: String -> Maybe (String, String)
splitHalf input = let strLen = length input in
                      let (quotient, rem') = strLen `divMod` 2 in
                          if rem' /= 0 then Nothing else Just $ splitHalf' 0 quotient strLen input
--splitHalf input = let strLen = length input in splitHalf' 0 (strLen / 2) strLen input
  where
    splitHalf' _ _ _ [] = ([], [])
    splitHalf' pos half len (x:xs) | pos >= half = let (left, right) = splitHalf' (pos + 1) half len xs in (left, x:right)
    splitHalf' pos half len (x:xs) = let (left, right) = splitHalf' (pos + 1) half len xs in (x:left, right)

-- commonPriorities :: [String] -> [Integer]
-- commonPriorities input = let strLen = length input in


main :: IO ()
main = do
  print $ splitHalf "abcd"
  -- file <- readFile "data/day3.hs"
  -- print $ sum $ commonPriorities $ lines file
