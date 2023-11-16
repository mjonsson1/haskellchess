module Solver where

import Chess

whoHasWon :: Game -> Maybe Winner
whoHasWon (board, side, turn)
  | length kingList == 2 && turn /= 0 = Nothing
  | length kingList == 1 = Just (WinningSide winningSide)
  | otherwise = Just Tie
  where
    kingList = filter (\((_, _), (pType, _)) -> pType == King) board
    ((_, _), (_, winningSide)) = head kingList