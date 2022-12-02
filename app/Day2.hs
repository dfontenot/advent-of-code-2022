module Main where

import Text.ParserCombinators.Parsec hiding (State)
import Control.Monad (void)

data RoundResult = Win | Lose | Draw
data RPC = Rock | Paper | Scissors
type Round = (RPC, RPC)
type Parsed = [Round]

opponentMove :: Parser RPC
opponentMove = do
  move <- choice [char 'A', char 'B', char 'C']
  return $ translateMove move
    where
      translateMove 'A' = Rock
      translateMove 'B' = Paper
      translateMove 'C' = Scissors
      translateMove _ = error "opponent move parse fail"

yourMove :: Parser RPC
yourMove = do
  move <- choice [char 'X', char 'Y', char 'Z']
  return $ translateMove move
    where
      translateMove 'X' = Rock
      translateMove 'Y' = Paper
      translateMove 'Z' = Scissors
      translateMove _ = error "your move parse fail"

rpcLine :: Parser Round
rpcLine = do
  opponent <- opponentMove
  void $ many1 $ char ' '
  yours <- yourMove
  return (opponent, yours)

rpcFile :: Parser Parsed
rpcFile = rpcLine `endBy1` char '\n'

parseInput :: String -> Either ParseError Parsed
parseInput = parse rpcFile "day2-part1.txt" -- 2nd arg is just the filename to use in parseerror s

-- TODO: better way...
roundResult :: Round -> RoundResult
roundResult (Rock, Rock) = Draw
roundResult (Rock, Paper) = Win
roundResult (Rock, Scissors) = Lose
roundResult (Paper, Rock) = Lose
roundResult (Paper, Paper) = Draw
roundResult (Paper, Scissors) = Win
roundResult (Scissors, Rock) = Win
roundResult (Scissors, Paper) = Lose
roundResult (Scissors, Scissors) = Draw

rpcMoveScore :: RPC -> Integer
rpcMoveScore Rock = 1
rpcMoveScore Paper = 2
rpcMoveScore Scissors = 3

roundScore :: RoundResult -> RPC -> Integer
roundScore result rpc = let moveScore = rpcMoveScore rpc in
                            case result of
                              Win -> moveScore + 6
                              Draw -> moveScore + 3
                              Lose -> moveScore

totalScore :: Parsed -> Integer
totalScore moves = sum $ map roundScore' moves
  where
    roundScore' (opponent, yours) = roundScore (roundResult (opponent, yours)) yours

main :: IO ()
main = do
  putStrLn "go"
  fileInput <- readFile "./data/day2-part1.txt"
  let parsed = parseInput fileInput in
      case parsed of
        Right result -> print $ totalScore result
        Left err -> print err
