module Main where

import Text.ParserCombinators.Parsec hiding (State)
import Control.Monad (void)
import Data.Interval

type SectionAssignment = Interval Integer
type ElfPairAssignments = (SectionAssignment, SectionAssignment)
type Parsed = [ElfPairAssignments]

integer :: Parser Integer
integer = read <$> many1 digit

assignment :: Parser SectionAssignment
assignment = do
  lhs <- integer
  void $ char '-'
  rhs <- integer
  return $ Finite lhs <=..<= Finite rhs

assignmentLine :: Parser ElfPairAssignments
assignmentLine = do
  elf1Assignment <- assignment
  void $ char ','
  elf2Assignment <- assignment
  return (elf1Assignment, elf2Assignment)

assignmentFile :: Parser Parsed
assignmentFile = assignmentLine `endBy1` char '\n'

parseInput :: String -> Either ParseError Parsed
parseInput = parse assignmentFile "day4.txt" -- 2nd arg is just the filename to use in parseerror s

fullyConsumed :: ElfPairAssignments -> Bool
fullyConsumed (lhs, rhs) = lhs `isSubsetOf` rhs || rhs `isSubsetOf` lhs

main :: IO ()
main = do
  putStrLn "go"
  fileInput <- readFile "./data/day4.txt"
  let parsed = parseInput fileInput in
      case parsed of
        Right result -> print $ length $ filter fullyConsumed result
        Left err -> print err
