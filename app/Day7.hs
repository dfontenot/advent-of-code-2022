{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Text.ParserCombinators.Parsec hiding (State)
import Control.Monad (void, foldM)
import Control.Monad.State
import Control.Monad.Writer
import Data.List
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map
import Data.Graph (Graph, Tree (Node), Vertex, dfs, graphFromEdges)
import Data.Semigroup (Sum)

-- used for parsing only
data CommandName = ChangeDir | ListDir
data EntityStartToken = DirToken | FileSizeToken Int

-- used to represent the filesystem tree (as an acyclic graph)
newtype VirtualPath = VirtualPath { asPathParts :: [String] } deriving (Eq, Ord) -- head is the deepest part of the path
data File = File { name :: String, size :: Int }
data GraphNode = GraphNode { files :: [File], neighbors :: [VirtualPath] }
type NodeMap = Map.Map VirtualPath GraphNode
type FromVertexFunction = Vertex -> ([File], VirtualPath, [VirtualPath])
type ToVertexFunction = VirtualPath -> Maybe Vertex
type FileGraph = (Graph, FromVertexFunction, ToVertexFunction)

instance Show File where
  show File { name=name_, size=size_ } = "<File " ++ name_ ++ " size " ++ show size_ ++ ">"

instance Show GraphNode where
  show GraphNode { files=files_, neighbors=neighbors_ } = "<Node\n" ++
    foldl (\acc file -> show file ++ "\n" ++ acc) "" files_ ++ "\n" ++
      foldl (\acc neighbor -> show neighbor ++ "\n" ++ acc) "" neighbors_ ++ "\n>"

instance Show VirtualPath where
  show (VirtualPath path) = intercalate "/" (reverse path)

rootPath :: VirtualPath
rootPath = VirtualPath [""]

cdUpPath :: VirtualPath -> VirtualPath
cdUpPath (VirtualPath path) = VirtualPath $ tail path

cdDirPath :: VirtualPath -> String -> VirtualPath
cdDirPath (VirtualPath path) dir = VirtualPath $ dir:path

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

collectFiles :: Parsed -> State VirtualPath NodeMap
collectFiles cmds = collectFiles' cmds Map.empty
  where collectFiles' :: Parsed -> NodeMap -> State VirtualPath NodeMap
        collectFiles' [] files = return files
        collectFiles' (Cd GoToRoot:rst) files = put rootPath >> collectFiles' rst files
        collectFiles' (Cd Up:rst) files = get >>= \p -> put (cdUpPath p) >> collectFiles' rst files
        collectFiles' (Cd (Down dirName):rst) files = get >>= \p -> put (cdDirPath p dirName) >> collectFiles' rst files
        collectFiles' (Ls entities:rst) files = get >>= \p -> collectFiles' rst $ Map.insert p (collectLsContents p entities) files
        collectLsContents :: VirtualPath -> [Entity] -> GraphNode
        collectLsContents path entities = collectLsContents' path entities ([], [])
        collectLsContents' :: VirtualPath -> [Entity] -> ([File], [VirtualPath]) -> GraphNode
        collectLsContents' path [] (files, neighbors) = GraphNode {files=files, neighbors=neighbors}
        collectLsContents' path ((Fn size name):rst) (files, neighbors) = collectLsContents' path rst (File {name=name, size=size}:files, neighbors)
        collectLsContents' path ((Dir name):rst) (files, neighbors) = collectLsContents' path rst (files, cdDirPath path name:neighbors)

graphFromNodeMap :: NodeMap -> FileGraph
graphFromNodeMap nm = graphFromEdges edges
  where
    edges = map (\ (k, v) -> (files v, k, neighbors v)) $ Map.toList nm

fsTree :: FileGraph -> Tree Vertex
fsTree (graph, _, toVertexFnc) = head $ dfs graph [fromJust (toVertexFnc rootPath)]

filesFromVertex :: FromVertexFunction -> Vertex -> [File]
filesFromVertex fnc v = let (files, _, _) = fnc v in files

maxDirSize :: Int
maxDirSize = 100000

walkFsTree :: FileGraph -> Writer [Int] Int
walkFsTree (graph, fromVertexFnc, toVertexFnc) = walkFsTree' $ fsTree (graph, fromVertexFnc, toVertexFnc)
  where
    walkFsTree' :: Tree Vertex -> Writer [Int] Int
    walkFsTree' (Node filesVertex []) = let dirSize = filesSizes filesVertex in
                                            if dirSize <= maxDirSize then tell [dirSize] >> return dirSize else return dirSize
    walkFsTree' (Node filesVertex neighbors) = let dirSize = filesSizes filesVertex in do
      neighborSizes <- foldM (\acc neighbor -> (+) <$> walkFsTree' neighbor <*> pure acc) dirSize neighbors
      if neighborSizes <= maxDirSize then do
                                     tell [neighborSizes]
                                     return neighborSizes
                                     else return neighborSizes
    filesSizes filesVertex = foldl (\a f -> a + size f) 0 (filesFromVertex fromVertexFnc filesVertex)

main :: IO ()
main = do
  putStrLn "go"
  fileInput <- readFile "./data/day7-test.txt"
  let parsed = parseInput fileInput in
      case parsed of
        Right result -> let nodeMap = evalState (collectFiles result) rootPath in
                            let fileGraph = graphFromNodeMap nodeMap in
                                let relevantSizes = execWriter (walkFsTree fileGraph) in do
                                  print $ sum relevantSizes
        Left err -> print err
