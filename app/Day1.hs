module Day1 where

import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (many1, skipMany1)
import Text.ParserCombinators.Parsec hiding (many, State)
import Text.Parsec.Char (digit, oneOf, char)

type Parsed = [[Integer]]

integer :: Parser Integer
integer = read <$> many1 digit

caloriesFile :: Parser Parsed
caloriesFile = do
  calories <- integer `sepBy` char '\n'
  return (calories:(try ((skipMany1 (char '\n')) >> caloriesFile) <|> (return [])))

parseInput :: String -> Either ParseError Parsed
parseInput = parse caloriesFile "day1-part1.txt" -- 2nd arg is just the filename to use in parseerror s

main :: IO ()
main = do
  fileInput <- readFile "../data/day1-part1.txt"
  let parsed = parseInput fileInput in
      case parsed of
        Right result -> print result
        Left err -> print err
