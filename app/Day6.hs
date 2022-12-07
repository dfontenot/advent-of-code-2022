module Main where

import Control.Monad.State
import qualified Data.Set as Set

type ProcessingState = (Int, String)

-- how many chars in a row that must be unique
numOfUnique :: Int
numOfUnique = 4

unique :: String -> Bool
unique [] = True
unique lst = let set = Set.fromList lst in
                 Set.size set == length lst

uniqueScan :: String -> State ProcessingState (Maybe Int)
uniqueScan [] = return Nothing
uniqueScan (x:xs) = do
  (pos, window) <- get
  case unique window of
    True -> return $ Just pos
    False -> let newWindow = (tail window) ++ [x] in
                 (put (pos + 1, newWindow)) >> uniqueScan xs

runScan :: String -> Maybe Int
runScan str | length str < numOfUnique = Nothing
runScan str | length str == numOfUnique = if unique str then Just numOfUnique else Nothing
runScan str = evalState (uniqueScan (drop numOfUnique str)) (numOfUnique, take numOfUnique str)

main :: IO ()
main = do
  putStrLn "go"
  fileInput <- readFile "./data/day6.txt"
  case runScan fileInput of
    Just result -> print result
    Nothing -> putStrLn "no unique 4 chars in a row"
