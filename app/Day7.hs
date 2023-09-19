{-# LANGUAGE CPP #-}
module Main where

import Text.ParserCombinators.Parsec hiding (State)
import Control.Monad (void)
import Control.Monad.State
import Data.List
import qualified Data.Map.Strict as Map

-- used for parsing only
data CommandName = ChangeDir | ListDir
data EntityStartToken = DirToken | FileSizeToken Int

-- used to represent the filesystem tree (as an acyclic graph)
newtype VirtualPath = VirtualPath { asPathParts :: [String] } deriving (Eq, Ord) -- head is the deepest part of the path
data File = File { name :: String, size :: Int }
data Node = Node { files :: [File], neighbors :: Map.Map VirtualPath Node }
newtype AdjMatrix = AdjMatrix { asPathMap :: Map.Map VirtualPath Node }

instance Show File where
  show File { name=name_, size=size_ } = "<File " ++ name_ ++ " size " ++ show size_ ++ ">"

instance Show Node where
  show Node { files=files_, neighbors=neighbors_ } = "<Node\n" ++
    foldl (\acc file -> show file ++ "\n" ++ acc) "" files_ ++ "\n" ++
      foldl (\acc neighbor -> show neighbor ++ "\n" ++ acc) "" (Map.keys neighbors_) ++ "\n>"

instance Show AdjMatrix where
  show (AdjMatrix mat) = "<AdjMatrix\n" ++
    foldl (\acc (k, v) -> show k ++ " -> " ++ show v ++ "\n" ++ acc) "" (Map.toList mat) ++ "\n>"

instance Show VirtualPath where
  show (VirtualPath path) = intercalate "/" (reverse path)

rootPath :: VirtualPath
rootPath = VirtualPath [""]

cdUpPath :: VirtualPath -> VirtualPath
cdUpPath (VirtualPath path) = VirtualPath $ tail path

cdDirPath :: VirtualPath -> String -> VirtualPath
cdDirPath (VirtualPath path) dir = VirtualPath $ dir:path

-- some matrix manipulation functions
emptyNode :: Node
emptyNode = Node { files = [], neighbors = Map.empty }

addFileToNode :: Node -> Entity -> Node
addFileToNode n (Dir _) = n
addFileToNode Node { files=files_, neighbors=neighbors_ } (Fn size_ fn) = Node { files = File { name=fn, size=size_ }:files_, neighbors=neighbors_ }

addFilesToNode :: Node -> [Entity] -> Node
addFilesToNode = foldl addFileToNode

addFilesAtPath :: AdjMatrix -> VirtualPath -> [Entity] -> AdjMatrix
addFilesAtPath (AdjMatrix mat) path entities = AdjMatrix $ Map.insert path (addFilesToNode (Map.findWithDefault emptyNode path mat) entities) mat

addEntityToNode :: Node -> VirtualPath -> Entity -> Node
addEntityToNode Node { files=files_, neighbors=neighbors_ } path (Dir dirName) =
  Node { files = files_, neighbors=Map.insert (cdDirPath path dirName) emptyNode neighbors_ }
addEntityToNode Node { files=files_, neighbors=neighbors_ } path (Fn size_ fn) =
  Node { files = File { name=fn, size=size_ }:files_, neighbors=neighbors_ }

addEntitiesToNode :: Node -> VirtualPath -> [Entity] -> Node
addEntitiesToNode node path = foldl (`addEntityToNode` path) node

addEntitiesAtPath :: AdjMatrix -> VirtualPath -> [Entity] -> AdjMatrix
addEntitiesAtPath (AdjMatrix mat) path entities = AdjMatrix $ Map.insert path (addEntitiesToNode (Map.findWithDefault emptyNode path mat) path entities) mat

-- used for building the adjacency matrix
type MatrixBuilderState = (AdjMatrix, VirtualPath)

instance Show CommandName where
  show ChangeDir = "cd"
  show ListDir = "ls"

instance Show EntityStartToken where
  show DirToken = "dir"
  show (FileSizeToken size) = show size

-- used to construct the instructions
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
  show (Fn size fnName) = show size ++ " " ++ fnName

instance Show Command where
  show (Cd direction) = "$ cd " ++ show direction
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
  void $ char ' '
  entityName <- filenameUntilEndl
  return $ case start of
    DirToken -> Dir entityName
    FileSizeToken size -> Fn size entityName

lsLastNewline :: Parser Bool
lsLastNewline = do
  optional newline
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

filenameChar :: Parser Char
filenameChar = alphaNum <|> oneOf "."

filenameUntilEndl :: Parser String
filenameUntilEndl = filenameChar `manyTill` lookAhead newlineOrEof

cdArg :: Parser CdDirection
cdArg = do
  void $ many1 $ char ' '
  dir <- filenameUntilEndl <|> string "/"
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
commandsFile = commandWithOutput `endBy` newlineOrEof

parseInput :: String -> Either ParseError Parsed
parseInput = parse commandsFile "day7.txt" -- 2nd arg is just the filename to use in parseerror s

buildAdjacencyMatrix :: Parsed -> State MatrixBuilderState AdjMatrix
buildAdjacencyMatrix [] = get >>= \ (m, _) -> return m
buildAdjacencyMatrix (Cd GoToRoot:rst) = gets fst >>= \m -> put (m, rootPath) >> buildAdjacencyMatrix rst
buildAdjacencyMatrix (Cd Up:rst) = get >>= \ (m, path) -> put (m, cdUpPath path) >> buildAdjacencyMatrix rst
buildAdjacencyMatrix (Cd (Down dirName):rst) = do
  (mat_, path) <- get
  let newPath = cdDirPath path dirName in do
    let mat = asPathMap mat_ in do
      case Map.lookup newPath mat of
        Nothing -> put (AdjMatrix (Map.insert newPath emptyNode mat), newPath)
        Just _ -> put (AdjMatrix mat, newPath)
      buildAdjacencyMatrix rst
buildAdjacencyMatrix (Ls entities:rst) = do
  (mat, path) <- get
  put (addEntitiesAtPath mat path entities, path)
  buildAdjacencyMatrix rst

main :: IO ()
main = do
  putStrLn "go"
  fileInput <- readFile "./data/day7.txt"
  let parsed = parseInput fileInput in
      case parsed of
        --Right result -> putStrLn ("results: " ++ show (length result)) >> print result
        Right result -> let mat = evalState (buildAdjacencyMatrix result) (AdjMatrix Map.empty, rootPath) in do
          print mat
        Left err -> print err
