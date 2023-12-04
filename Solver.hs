module Solver where

import Chess
import Debug.Trace

-- Takes a game and return the outcome of either someone winning or a tie
whoWillWin :: Game -> Winner
whoWillWin game@(_, side, _) =
  case whoHasWon game of
    -- Base case: someone won
    Just winningState -> winningState
    -- Recursive case: consider the winner of all possible games
    Nothing ->
      let allNextGame = [makeUnSafeMove game move | move <- allLegalMoves game]
          -- calculate a bunch of
          winners :: [Winner]
          winners = [whoWillWin nextGame | nextGame <- allNextGame]
          winner
            | all (== WinningSide (opponent side)) winners = WinningSide (opponent side)
            | (WinningSide side) `elem` winners = WinningSide side
            | otherwise = Tie
       in winner

-- Generate an association list of all the next possible game states and the moves that lead to each of them
gameMoveAssociation :: Game -> [(Game, Move)]
gameMoveAssociation game@(board, side, turn) =
  let allMoves = allLegalMoves game
   in [(makeUnSafeMove game move, move) | move <- allMoves]

-- Generate the best move for the current plauer
bestMove :: Game -> Move
bestMove game@(_, side, _) =
  let outcomes :: [(Winner, Move)]
      outcomes = [(whoWillWin g, m) | tup@(g, m) <- gameMoveAssociation game]
   in case lookup (WinningSide side) outcomes of
        -- If there is a move which leads to the current player winnning: take it
        Just move -> move
        -- Else
        Nothing ->
          case lookup Tie outcomes of
            -- If there is a move which leads to a tie: take it
            Just move -> move
            -- Otherwise just take the first move
            Nothing -> snd (head outcomes)

-- TODO: Breadth first find best move maybe, so you can check mate in 1 instead of 5