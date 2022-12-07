module Main where

import Control.Monad.State
import qualified Data.Set as Set

type ProcessingState = (Integer, String)

unique :: String -> Bool
unique [] = True
unique lst = let set = Set.fromList lst in
                 Set.size set == length lst

uniqueScan :: String -> State ProcessingState (Maybe Integer)
uniqueScan [] = return Nothing
uniqueScan (x:xs) = do
  (pos, window) <- get
  case unique window of
    True -> return $ Just pos
    False -> let newWindow = (tail window) ++ [x] in
                 (put (pos + 1, newWindow)) >> uniqueScan xs

runScan :: String -> Maybe Integer
runScan str | length str < 4 = Nothing
runScan str | length str == 4 = if unique str then Just 4 else Nothing
runScan str = evalState (uniqueScan (drop 4 str)) (4, take 4 str)

main :: IO ()
main = do
  putStrLn "go"
  fileInput <- readFile "./data/day6.txt"
  case runScan fileInput of
    Just result -> print result
    Nothing -> putStrLn "no unique 4 chars in a row"
