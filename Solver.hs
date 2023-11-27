module Solver where

import Chess
import Debug.Trace

allNextGame :: Game -> [Game]
allNextGame game@(board, side, turn) =
  let allMoves = allLegalMoves game
  in [makeUnSafeMove game move | move <- allMoves]



whoWillWin :: Game -> Winner
whoWillWin game@(_, side, _) =
  case whoHasWon game of
    -- winning State can be a tie or a WinningSide side
    Just winningState -> winningState
    Nothing ->
      let nextGameWinners :: [Winner]
          nextGameWinners = [whoWillWin nextGame | nextGame <- allNextGame game]
          determineWinner :: [Winner] -> Winner
          determineWinner winners
            | all (== WinningSide (opponent side)) winners = WinningSide (opponent side)
            | (WinningSide side) `elem` winners = WinningSide side
            | otherwise = Tie
       in determineWinner nextGameWinners

gameMoveAssociation :: Game -> [(Game, Move)]
gameMoveAssociation game@(board, side, turn) =
  let allMoves = allLegalMoves game
  in [(makeUnSafeMove game move, move) | move <- allMoves]

bestMove :: Game -> Move
bestMove game@(board, side, _) =
  let outcomes :: [(Winner,  Move)]
      outcomes = [(whoWillWin g, m) | tup@(g, m) <- gameMoveAssociation game]
  in case lookup (WinningSide side) outcomes of 
    Just move -> move
    Nothing -> 
      case lookup Tie outcomes of 
        Nothing -> snd (head outcomes)
        Just move -> move



testBoard :: Board
testBoard = [((8, 8), (King, White)), ((1, 1), (King, Black)), ((7, 2), (Rook, Black)), ((6, 1), (Rook, Black))]
      

--TODO: Breadth first find best move maybe, so you can check mate in 1 instead of 5