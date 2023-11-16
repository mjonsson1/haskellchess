module Solver where

import Chess

whoHasWon :: Game -> Maybe Winner
whoHasWon (board, side, turn) =
    | length kingList == 2 && turn /= 0 = Nothing
    | turn == 0 = Just Tie
    | otherwise = Just WinningSide winningSide
    where kingList = filter (\((_, _), (pType, _)) -> pType == King) board
        ((_, _), (_, winningSide)) = head kingList