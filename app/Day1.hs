module Main where

import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (many1, skipMany1)
import Text.ParserCombinators.Parsec hiding (many, State)
import Text.Parsec.Char (digit, oneOf, char)
import Control.Monad (void)

type Parsed = [[Integer]]

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
--caloriesFile = calories `sepBy` char '\n'
caloriesFile = manyTill calories (try (choice [eof, void (char '\n')]))

parseInput :: String -> Either ParseError Parsed
parseInput = parse caloriesFile "day1-part1.txt" -- 2nd arg is just the filename to use in parseerror s

main :: IO ()
main = do
  putStrLn "go"
  fileInput <- readFile "./data/day1-part1.txt"
  let parsed = parseInput fileInput in
      case parsed of
        Right result -> print result
        Left err -> print err
