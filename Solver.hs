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

pieceValue :: PieceType -> Int
pieceValue Pawn = 1
pieceValue Bishop = 3
pieceValue Knight = 3
pieceValue Rook = 5
pieceValue Queen = 9
pieceValue King = 10000

-- Optimized via folds
-- TODO: edge cases if the position is winning / losing
rateGame :: Game -> Int
rateGame (board, side, int) = 
    let (white, black) = foldr (\(_, (pieceType, side)) (white, black) -> if side == White then (white + pieceValue pieceType, black) else (white, black + pieceValue pieceType)) (0, 0) board
      in white - black