module Solver where

import Chess
import Debug.Trace
import Distribution.Fields.Field (getName)

generateAllNextGame :: Game -> [Game]
generateAllNextGame (board, side, turn) =
  let allAllySquares :: [Square]
      allAllySquares = filter (\(_, (_, pSide)) -> pSide == side) board
      allAllyLegalMoves :: [Move]
      allAllyLegalMoves = concat [allLegalMoves sq board | sq <- allAllySquares]
      allNextBoard = [makeUnSafeMove board move | move <- allAllyLegalMoves]
   in [(newBoard, opponent side, turn - 1) | newBoard <- allNextBoard]

-- already takes into account whether turn hit 0 or not
whoHasWon :: Game -> Maybe Winner
whoHasWon (board, side, turn)
  | length kingList == 2 && turn /= 0 = Nothing
  | length kingList == 1 = Just (WinningSide winningSide)
  | otherwise = Just Tie
  where
    kingList = filter (\((_, _), (pType, _)) -> pType == King) board
    ((_, _), (_, winningSide)) = head kingList

whoWillWin :: Game -> Winner
whoWillWin game@(_, side, _) =
  case whoHasWon game of
    -- winning State can be a tie or a WinningSide side
    Just winningState -> winningState
    Nothing ->
      let allNextGame = generateAllNextGame game
          nextGameWinners :: [Winner]
          nextGameWinners = [whoWillWin nextGame | nextGame <- allNextGame]
       in case nextGameWinners of
            winners
              | all (== WinningSide (opponent side)) winners -> WinningSide (opponent side)
              | (WinningSide side) `elem` winners -> WinningSide side
              | otherwise -> Tie

testBoard :: Board
testBoard = [((8, 8), (King, White)), ((1, 1), (King, Black)), ((7, 2), (Rook, Black)), ((6, 1), (Rook, Black))]
