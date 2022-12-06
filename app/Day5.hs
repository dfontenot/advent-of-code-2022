module Main where

import Text.ParserCombinators.Parsec hiding (State)
import Control.Monad (void)
import Control.Monad.State
import qualified Data.Vector as V

type Crate = Maybe Char
type Header = [[Crate]]
data Command = CreateMove Integer Integer Integer | SimpleMove Integer Integer
data Parsed = Parsed Header [Command]
data Gamestate = Gamestate (V.Vector String) [Command]

instance Show Command where
  show (CreateMove num source dest) = "<Command num=" ++ show num ++ " src=" ++ show source ++ " dest=" ++ show dest ++ ">"
  show (SimpleMove source dest) = "<Command src=" ++ show source ++ " dest=" ++ show dest ++ ">"

instance Show Parsed where
  show (Parsed header commands) = "<Parsed header=" ++ show header ++ " cmds=" ++ show commands ++ ">"

instance Show Gamestate where
  show (Gamestate stacks commands) = "<Gamestate stacks=" ++ show stacks ++ " cmds=" ++ show commands ++ ">"

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
parseHeaderLine = do
  crate <- choice [parseCrate, parseEmptySpot]
  nextChar <- oneOf " \n"
  case nextChar of
    ' ' -> do
      nextCrate <- parseHeaderLine
      return (crate:nextCrate)
    '\n' -> return [crate]
    _ -> unexpected "unexpected char when reading header line"

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

simplifyMoves :: [Command] -> [Command]
simplifyMoves [] = []
simplifyMoves ((SimpleMove src dest):xs) = SimpleMove src dest:simplifyMoves xs
simplifyMoves ((CreateMove num src dest):xs) = replicate (fromIntegral num) (SimpleMove src dest) ++ simplifyMoves xs

postProcessHeaderLine :: [Crate] -> [String] -> [String]
postProcessHeaderLine = zipWith zipper
  where
    zipper (Just c) line = line ++ [c]
    zipper Nothing line = line

initialStacks :: [[Crate]] -> V.Vector String
initialStacks [] = V.fromList [[]]
initialStacks [[]] = V.fromList [[]]
initialStacks stacks = let numStacks = length (head stacks) in
                           V.fromList $ foldr postProcessHeaderLine (replicate numStacks []) stacks

initialGamestate :: Parsed -> Gamestate
initialGamestate (Parsed header commands) = Gamestate (initialStacks header) (simplifyMoves commands)

-- TODO: shorter?
takeLast :: Int -> [a] -> [a]
takeLast 0 _ = []
takeLast n _ | n < 0 = error "no"
takeLast n lst' = takeLast' n (length lst') lst'
  where
    takeLast' 0 _ lst = lst
    takeLast' n' len lst | n' >= len = lst
    takeLast' n' len lst = takeLast' (n'-1) len (tail lst)

-- simulateCrane :: Gamestate -> String
-- simulateCrane (Gamestate _ []) = "TODO"
-- simulateCrane (Gamestate stacks (x:xs)) = simulateCrane (Gamestate (newStacks stacks x) xs)
--   where
--     newStacks stacks (CreateMove numCrates sourceCrate destCrate) = _

main :: IO ()
main = do
  putStrLn "go"
  fileInput <- readFile "./data/day5.txt"
  let parsed = parseInput fileInput in
      case parsed of
        Right result -> print $ initialGamestate result
        Left err -> print err
