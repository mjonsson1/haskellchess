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
          let winners :: [Winner]
              winners = [whoWillWin nextGame | nextGame <- allNextGame game]
              bestWinner
                | all (== WinningSide (opponent side)) winners = WinningSide (opponent side)
                | (WinningSide side) `elem` winners = WinningSide side
                | otherwise = Tie
          in bestWinner

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
