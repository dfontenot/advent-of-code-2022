module Main where

import Text.ParserCombinators.Parsec hiding (State)
import Control.Monad (void)
import Control.Monad.State

type Crate = Maybe Char
type Header = [[Crate]]
data Command = CreateMove Integer Integer Integer
data Parsed = Parsed Header [Command]

instance Show Command where
  show (CreateMove num source dest) = "<Command num=" ++ show num ++ " src=" ++ show source ++ " dest=" ++ show dest

instance Show Parsed where
  show (Parsed header commands) = "<Parsed header=" ++ show header ++ " cmds=" ++ show commands

integer :: Parser Integer
integer = read <$> many1 digit

parseCrate :: Parser Crate
parseCrate = do
  void $ char '['
  crateId <- upper
  void $ char ']'
  return $ Just crateId

parseEmptySpot :: Parser Crate
parseEmptySpot = do
  void $ string "   "
  return Nothing

parseHeaderLine :: Parser [Crate]
parseHeaderLine = manyTill (choice [parseCrate, parseEmptySpot]) (try newline)

parseHeader :: Parser Header
parseHeader = do
  headerLines <- manyTill parseHeaderLine (try (char ' '))
  void $ many1 $ oneOf " 0123456789"
  void $ many1 newline
  return headerLines

parseCommand :: Parser Command
parseCommand = do
  void $ string "move "
  numCrates <- integer
  void $ string " from "
  sourceCrate <- integer
  void $ string " to "
  destCrate <- integer
  return $ CreateMove numCrates sourceCrate destCrate

parseCommands :: Parser [Command]
parseCommands = parseCommand `endBy1` newline

commandsFile :: Parser Parsed
commandsFile = do
  header <- parseHeader
  Parsed header <$> parseCommands

parseInput :: String -> Either ParseError Parsed
parseInput = parse commandsFile "day5.txt"

main :: IO ()
main = do
  putStrLn "go"
  fileInput <- readFile "./data/day5.txt"
  let parsed = parseInput fileInput in
      case parsed of
        Right result -> print result
        Left err -> print err
