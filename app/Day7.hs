module Main where

import Text.ParserCombinators.Parsec hiding (State)
import Control.Monad (void)
import Data.Tree
import Data.List

-- used for parsing only
data CommandName = ChangeDir | ListDir
data EntityStartToken = DirToken | FileSizeToken Int

-- used to construct the final tree
data CdDirection = Up | GoToRoot | Down String
data Entity = Dir String | Fn Int String -- directory with name, or file name with size
data Command = Cd CdDirection | Ls [Entity]
type Parsed = [Command]

instance Show CdDirection where
  show Up = ".."
  show GoToRoot = "/"
  show (Down path) = path

instance Show Entity where
  show (Dir dirName) = "dir " ++ dirName
  show (Fn size fnName) = show size ++ fnName

instance Show Command where
  show (Cd direction) = "$ cd" ++ show direction
  show (Ls output) = "$ ls\n" ++ intercalate "\n" (map show output)

integer :: Parser Int
integer = read <$> many1 digit

lsEntityStart :: Parser EntityStartToken
lsEntityStart = choice [parseDir, parseFileSize]
  where
    parseDir = void (string "dir") >> return DirToken
    parseFileSize = integer >>= \i -> return $ FileSizeToken i

lsEntity :: Parser Entity
lsEntity = do
  start <- lsEntityStart
  void $ char ' '
  entityName <- filenameUntilEndl
  return $ case start of
    DirToken -> Dir entityName
    FileSizeToken size -> Fn size entityName

-- TODO: handle EOF
lsOutput :: Parser [Entity]
lsOutput = lsEntity `manyTill` try (newline >> char '$')
-- lsOutput = do
--   entities <- lsEntity `manyTill` lookAhead (newline >> char '$')
--   void newline
--   return entities

filenameChar :: Parser Char
filenameChar = alphaNum <|> oneOf "."

filenameUntilEndl :: Parser String
filenameUntilEndl = filenameChar `manyTill` newline

cdArg :: Parser CdDirection
cdArg = do
  void $ many1 $ char ' '
  dir <- filenameUntilEndl <|> (string "/" >>= \path -> return path)
  return $ case dir of
             ".." -> Up
             "/" -> GoToRoot
             _ -> Down dir

commandName :: Parser CommandName
commandName = choice [parseCd, parseLs]
  where
    parseCd = void (string "cd") >> return ChangeDir
    parseLs = void (string "ls") >> return ListDir

commandWithOutput :: Parser Command
commandWithOutput = do
  void $ string "$ "
  cmd <- commandName
  case cmd of
    ChangeDir -> cdArg >>= \dirName -> return $ Cd dirName
    ListDir -> void newline >> lsOutput >>= \output -> return $ Ls output

commandsFile :: Parser Parsed
commandsFile = commandWithOutput `endBy` char '\n'

parseInput :: String -> Either ParseError Parsed
parseInput = parse commandsFile "day7.txt" -- 2nd arg is just the filename to use in parseerror s

main :: IO ()
main = do
  putStrLn "go"
  fileInput <- readFile "./data/day7.txt"
  let parsed = parseInput fileInput in
      case parsed of
        Right result -> print result
        Left err -> print err
