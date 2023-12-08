module Solver where

import Chess
import Data.List
import Data.Maybe
import Debug.Trace

--                                                    HELPER FUNCTIONS
allNextGame :: Game -> [Game]
allNextGame game@(board, side, turn) =
  let allMoves = allLegalMoves game
   in [makeUnSafeMove game move | move <- allMoves]

-- Generate an association list of all the next possible game states and the moves that lead to each of them
gameMoveAssociation :: Game -> [(Game, Move)]
gameMoveAssociation game@(board, side, turn) =
  let allMoves = allLegalMoves game
   in [(makeUnSafeMove game move, move) | move <- allMoves]

--                                                     SPRINT 2

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

-- Generate the best move for the current player
-- Does not have dynamic depth
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

--                                                     SPRINT 3

typeValue :: PieceType -> Int
typeValue Pawn = 1
typeValue Bishop = 3
typeValue Knight = 3
typeValue Rook = 5
typeValue Queen = 9
typeValue King = 10000

pieceValue (pieceType, White) = typeValue pieceType
pieceValue (pieceType, Black) = -(typeValue pieceType)

-- Optimized via foldsshow
rateGame :: Game -> Rating
rateGame (board, side, int) =
  foldr (\(_, piece) total -> total + pieceValue piece) 0 board

moveEstimate :: Game -> Int -> (Rating, Maybe Move)
moveEstimate game@(_, player, turn) remDepth
  | remDepth == 0 || isJust (whoHasWon game) = (rateGame game, Nothing)
  | otherwise =
      let ratingMoveAssociation = [(fst (moveEstimate nextGame (remDepth - 1)), Just nextMove) | (nextGame, nextMove) <- gameMoveAssociation game]
       in selectFor player ratingMoveAssociation

selectFor :: Side -> [(Rating, Maybe Move)] -> (Rating, Maybe Move)
selectFor _ [r] = r
selectFor White (r : rs) =
  if fst r > 1000
    then r
    else max r (selectFor White rs)
selectFor Black (r : rs) =
  if fst r < -1000
    then r
    else min r (selectFor Black rs)