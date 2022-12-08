{-# LANGUAGE CPP #-}
module Main where

import Text.ParserCombinators.Parsec hiding (State)
import Control.Monad (void)
import Debug.Trace
import Data.Tree
import Data.List

-- used for parsing only
data CommandName = ChangeDir | ListDir
data EntityStartToken = DirToken | FileSizeToken Int

instance Show CommandName where
  show ChangeDir = "cd"
  show ListDir = "ls"

instance Show EntityStartToken where
  show DirToken = "dir"
  show (FileSizeToken size) = show size

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

newlineOrEof :: Parser ()
newlineOrEof = void newline <|> eof

lsEntityStart :: Parser EntityStartToken
lsEntityStart = choice [parseDir, parseFileSize]
  where
    parseDir = void (string "dir") >> return DirToken
    parseFileSize = integer >>= \i -> return $ FileSizeToken i

lsEntity :: Parser Entity
lsEntity = do
  start <- lsEntityStart
  trace ("in lsEntity " ++ show start) $ void $ char ' '
  entityName <- filenameUntilEndl
  return $ case trace ("read filename '" ++ show entityName ++ "'") start of
    DirToken -> trace ("dir name " ++ entityName) $ Dir entityName
    FileSizeToken size -> trace ("found file " ++ show size ++ " " ++ entityName) $ Fn size entityName

lsLastNewline :: Parser Bool
--lsLastNewline = try (newline >> char '$' >> return True) <|> return False
--lsLastNewline = choice [eof >> return True, try (newline >> char '$' >> return True) <|> return False]
lsLastNewline = do
  void newline
  -- TODO: super ugly, just choosing a random char for the fallback for the try, refactor to get rid of the need to do that
  choice [eof >> return True, lookAhead (try (char '$') <|> return '^') >>= \char -> return (char == '$')]

-- TODO: handle EOF
lsOutput :: Parser [Entity]
--lsOutput = many1 lsEntity -- TODO: consume newline too
lsOutput = do
  entity <- lsEntity
  isLastNewline <- lookAhead lsLastNewline
  if not isLastNewline
     then do
       newlineOrEof
       next <- lsOutput
       return $ entity:next
    else return [entity]
--lsOutput = lsEntity `manyTill` lookAhead ((void (char '$')) <|> eof)
--lsOutput = lsEntity `manyTill` lookAhead (char '$') <|> (eof >> return [])
--lsOutput = lsEntity `manyTill` lookAhead (char '$')
-- lsOutput = do
--   entities <- lsEntity `manyTill` lookAhead (char '$')
--   void newline
--   return entities
-- lsOutput = do
--   entities <- lsEntity `manyTill` lookAhead (newline >> char '$')
--   void newline
--   return entities

filenameChar :: Parser Char
filenameChar = alphaNum <|> oneOf "."

filenameUntilEndl :: Parser String
filenameUntilEndl = filenameChar `manyTill` lookAhead newlineOrEof

cdArg :: Parser CdDirection
cdArg = do
  void $ many1 $ char ' '
  dir <- filenameUntilEndl <|> string "/"
  --dir <- filenameUntilEndl <|> (string "/" >>= \path -> return path)
  --void newline
  return $ case trace ("dir found was " ++ dir) dir of
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
  case trace ("cmd was " ++ show cmd) cmd of
    ChangeDir -> cdArg >>= \dirName -> return $ Cd dirName
    ListDir -> void newline >> lsOutput >>= \output -> return $ Ls output

commandsFile :: Parser Parsed
commandsFile = commandWithOutput `endBy` newlineOrEof

parseInput :: String -> Either ParseError Parsed
parseInput = parse commandsFile "day7.txt" -- 2nd arg is just the filename to use in parseerror s

main :: IO ()
main = do
  putStrLn "go"
  fileInput <- readFile "./data/day7.txt"
  let parsed = parseInput fileInput in
      case parsed of
        Right result -> putStrLn ("results: " ++ show (length result)) >> print result
        Left err -> print err
