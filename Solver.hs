module Solver where

import Chess
import Data.List
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

-- pieceValue :: PieceType -> Int
-- pieceValue Pawn = 1
-- pieceValue Bishop = 3
-- pieceValue Knight = 3
-- pieceValue Rook = 5
-- pieceValue Queen = 9
-- pieceValue King = 10000

-- -- Optimized via folds
-- -- TODO: edge cases if the position is winning / losing
-- rateGame :: Game -> Rating -- check if the game is won and also tie! if so, give highest possible marks
-- rateGame (board, side, int) =
--   let (white, black) = foldr (\(_, (pieceType, side)) (white, black) -> if side == White then (white + pieceValue pieceType, black) else (white, black + pieceValue pieceType)) (0, 0) board
--    in white - black

typeValue :: PieceType -> Int
typeValue Pawn = 1
typeValue Bishop = 3
typeValue Knight = 3
typeValue Rook = 5
typeValue Queen = 9
typeValue King = 10000

pieceValue (pieceType, White) = typeValue pieceType
pieceValue (pieceType, Black) = -(typeValue pieceType)

-- Optimized via folds
-- TODO: edge cases if the position is winning / losing
rateGame :: Game -> Rating
rateGame (board, side, int) =
  foldr (\(_, piece) total -> total + pieceValue piece) 0 board

-- foldr (\(_, (pieceType, side)) total -> if side == White then total + typeValue pieceType else total - typeValue pieceType) 0 board

whoMightWin :: Game -> Int -> (Rating, Maybe Move)
-- TODO turn error check so that we don't over analyze after turns are 0
whoMightWin game@(_, player, turn) remDepth
  | remDepth == 0 || (whoHasWon game /= Nothing) = (rateGame game, Nothing)
  | otherwise =
      selectFor
        player
        [(fst (whoMightWin nextGame (remDepth - 1)), Just nextMove) | (nextGame, nextMove) <- gameMoveAssociation game]

selectFor White = maximumBy (\(x1, _) (x2, _) -> compare x1 x2)
selectFor Black = minimumBy (\(x1, _) (x2, _) -> compare x1 x2)

{-
minimax :: Int -> GameState -> Int
minimax depth gameState
  | depth == 0 = rateGame (board gameState)
  | otherwise = if currentPlayer gameState == White
                  then maximum (map (minimax (depth - 1)) nextStates)
                  else minimum (map (minimax (depth - 1)) nextStates)
  where
    possibleMoves = generateMoves gameState -- Implement a function to generate all possible moves
    nextStates = map makeMove possibleMoves
    makeMove move = undefined -- Implement a function to apply a move to the game state
-}