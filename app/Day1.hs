module Main where

import Text.ParserCombinators.Parsec hiding (State)
import Control.Monad (void)
import Control.Monad.State

type Parsed = [[Integer]]
type TopThreeState = (Integer, Integer, Integer)

integer :: Parser Integer
integer = read <$> many1 digit

integerLine :: Parser Integer
integerLine = do
  int_ <- integer
  void $ char '\n'
  return int_

calories :: Parser [Integer]
calories = manyTill integerLine (try (choice [eof, void (char '\n')]))

caloriesFile :: Parser Parsed
caloriesFile = manyTill calories (try (choice [eof, void (char '\n')]))

parseInput :: String -> Either ParseError Parsed
parseInput = parse caloriesFile "day1-part1.txt" -- 2nd arg is just the filename to use in parseerror s

mostCalories :: Parsed -> Integer
mostCalories parsed = largest $ map sum parsed
  where
    largest = foldr max 0

topThreeCalories :: [Integer] -> State TopThreeState TopThreeState
topThreeCalories [] = get
topThreeCalories (x:xs) = do
  (top, second, third) <- get
  put $ update x top second third
  topThreeCalories xs
    where
      update x_ top second _ | x_ > top = (x_, top, second)
      update x_ top second _ | x_ > second = (top, x_, second)
      update x_ top second third | x_ > third = (top, second, x_)
      update _ top second third = (top, second, third)

runTopThreeCalories :: Parsed -> Integer
runTopThreeCalories parsed = sumTriple $ evalState (topThreeCalories (map sum parsed)) (0, 0, 0)
  where
    sumTriple (x, y, z) = x + y + z

mainPart1 :: IO ()
mainPart1 = do
  putStrLn "go"
  fileInput <- readFile "./data/day1-part1.txt"
  let parsed = parseInput fileInput in
      case parsed of
        Right result -> print $ mostCalories result
        Left err -> print err

main :: IO ()
main = do
  putStrLn "go"
  fileInput <- readFile "./data/day1-part1.txt"
  let parsed = parseInput fileInput in
      case parsed of
        Right result -> print $ runTopThreeCalories result
        Left err -> print err
