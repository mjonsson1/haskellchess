module Moves where 

import Chess
import Data.Maybe

--                                                 HELPER FUNCTIONS
inBound :: Int -> Bool
inBound x = x >= 1 && x <= 8



recurCheckPath :: Board -> Side -> Pos -> (Int, Int) -> [Pos]
recurCheckPath board side point dir --ix, iy are offsets -> a num in [-1..1]
  | not  $ inBound  (fst newPoint)  && inBound  (snd newPoint)  = []
  | otherwise =
      case lookup newPoint board of
        Nothing -> newPoint : recurCheckPath board side (newPoint) dir
        Just (piece, color) -> ([newPoint | color == opponent side])
  where newPoint = offset point dir

--                                            GENERATING ALL LEGAL MOVES
allLegalMoves :: Square -> Board -> [Move]
-- BISHOP
allLegalMoves square@((x, y), (Bishop, side)) board =
  let bishopDirections = [(-1, 1), (1, 1), (-1, -1), (1, -1)]
      allpos = concat [recurCheckPath board side (x, y) dir | dir <- bishopDirections]
  in [(square, pos) | pos <- allpos]

  
  -- let upLeft = recurCheckPath board side (x,y) (-1, 1)
  --     upRight = recurCheckPath board side (x,y) (1, 1)
  --     downLeft = recurCheckPath board side (x,y) (-1, -1)
  --     downRight = recurCheckPath board side (x,y) (1, -1)
  --     allPos = upLeft ++ upRight ++ downLeft ++ downRight
  --  in [(square, pos) | pos <- allPos]
-- ROOK
allLegalMoves square@((x, y), (Rook, side)) board =
  let rookDirections = [(-1, 0), (0, 1), (0, -1), (1, 0)]
      allpos = concat [recurCheckPath board side (x, y) dir | dir <- rookDirections]
  in [(square, pos) | pos <- allpos]
-- QUEEN
allLegalMoves square@((x, y), (Queen, side)) board =
  let queenDirections = [(-1, 0), (0, 1), (0, -1), (1, 0),(-1, 1), (1, 1), (-1, -1), (1, -1)]
      allpos = concat [recurCheckPath board side (x, y) dir | dir <- queenDirections]
  in [(square, pos) | pos <- allpos]
-- KNIGHT

allLegalMoves square@((x, y), (Knight, side)) board =
  let twoMove = [-2, 2]
      oneMove = [-1, 1]
      vertical :: [Pos]
      vertical = [ (x + i, y + j) | i <- oneMove, inBound (x + i), j <- twoMove, inBound (y + j)]
      horizontal = [ (x + j, y + i) | i <- oneMove, inBound (x + i), j <- twoMove, inBound (y + j)]
  in case side of
        White -> let legalSquares = filter (\pos -> not (isWhite (lookup pos board))) (vertical ++ horizontal)
          in [(square, pos) | pos <- legalSquares]
        Black -> let legalSquares = filter (\pos -> not (isBlack (lookup pos board))) (vertical ++ horizontal)
          in [(square, pos) | pos <- legalSquares]

--I rewrote this function for knight, but it does not operate as expected. FIX -Marco
{-
allLegalMoves square@((x, y), (Knight, side)) board =
  let twoMove = [-2, 2]
      oneMove = [-1, 1]
      vertical :: [Pos]
      vertical = [ (x + i, y + j) | i <- oneMove, inBound (x + i), j <- twoMove, inBound (y + j)]
      horizontal = [ (x + j, y + i) | i <- oneMove, inBound (x + i), j <- twoMove, inBound (y + j)]
      legalSquares = filter (\pos -> Just side == color (lookup pos board)) (vertical ++ horizontal)
      in [(square, pos) | pos <- legalSquares]
-}

-- KING
-- Also rewrote this one and it doesn't work as expected
{-
allLegalMoves square@((x, y), (King, side)) board =
  let oneMove = [-1, 0, 1]
      surrounding :: [Pos]
      surrounding = [ (x + i, y + j) | i <- oneMove, inBound (x + i), j <- oneMove, inBound (y + j)]
      surroundingWithoutStart = delete (x,y) surrounding
      legalSquares = filter (\pos -> Just side == color (lookup pos board)) (surroundingWithoutStart)
        in [(square, pos) | pos <- legalSquares]
-}

allLegalMoves square@((x, y), (King, side)) board =
  let oneMove = [-1, 0, 1]
      surrounding :: [Pos]
      surrounding = [ (x + i, y + j) | i <- oneMove, inBound (x + i), j <- oneMove, inBound (y + j)]
      surroundingWithoutStart = delete (x,y) surrounding
  in case side of
        White -> let legalSquares = filter (\pos -> not (isWhite (lookup pos board))) (surroundingWithoutStart)
          in [(square, pos) | pos <- legalSquares]
        Black -> let legalSquares = filter (\pos -> not (isBlack (lookup pos board))) (surroundingWithoutStart)
          in [(square, pos) | pos <- legalSquares]


-- PAWN
-- TODO: Ask Fogarty for reason why pattern matching didn't work and needed to be moved down
-- TODO: delete bound check for Pawn, bc if a y is at the edge: turn into queen
allLegalMoves square@((x, y), (Pawn, White)) board =
  let pushOnce = [(x, y + 1) | inBound (y + 1), isNothing (lookup (x, y + 1) board)]
      pushTwice = [(x, y + 2) | y == 2, isNothing (lookup (x, y + 2) board), isNothing (lookup (x, y + 1) board)]
      captures = [(x + dx, y + 1) | inBound (y + 1), dx <- [-1, 1], inBound (x + dx), isBlack (lookup (x + dx, y + 1) board)]
      allPos = pushOnce ++ pushTwice ++ captures
   in [(square, pos) | pos <- allPos]
allLegalMoves square@((x, y), (Pawn, Black)) board =
  let pushOnce = [(x, y - 1) | inBound (y - 1), isNothing (lookup (x, y - 1) board)]
      pushTwice = [(x, y - 2) | y == 7, isNothing (lookup (x, y - 2) board), isNothing (lookup (x, y - 1) board)]
      captures = [(x + dx, y - 1) | inBound (y - 1), dx <- [-1, 1], inBound (x + dx), isWhite (lookup (x + dx, y - 1) board)]
      allPos = pushOnce ++ pushTwice ++ captures
   in [(square, pos) | pos <- allPos]


-- TODO: lookup takes in a coordinate and outputs a piece tuple,
-- need to define a function to do the reverse, so that we can find the input position values to generate movess

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = False

isSomething :: Maybe a -> Bool
isSomething (Just _) = True
isSomething Nothing = False

isWhite :: Maybe Piece -> Bool
isWhite (Just (_, White)) = True
isWhite _ = False

isBlack :: Maybe Piece -> Bool
isBlack (Just (_, Black)) = True
isBlack _ = False

--NATE
-- given a piece and a position, return ALL POSSIBLE positions to move that piece (respecting bounds).
generateMoves :: (Pos, Piece) -> [Pos]
generateMoves ((x,y), (Rook, side)) =  generateRookMoves ((x,y), (Rook, side))
generateMoves ((x,y), (Bishop, side)) =  generateBishopMoves ((x,y), (Bishop, side))
generateMoves ((x,y), (Pawn, Black)) = undefined


-- generate a list of numbers in between two numbers not including them
numberInBetween :: Int -> Int -> [Int]
numberInBetween start end 
    | abs (start - end) <= 1 = []
    | start > end = (start - 1) : numberInBetween (start - 1) end
    | otherwise   = (start + 1) : numberInBetween (start + 1) end

isLegalMove :: Board -> Move -> Bool
isLegalMove board (((x,y), (p, s)), (x1,y1)) =
    -- can not move to the same square
    if (x == x1 && y == y1)
    then False
    else isLegalMoveAux board (((x,y), (p, s)), (x1,y1))
    
isLegalMoveAux :: Board -> Move -> Bool
isLegalMoveAux board (((x,y), (Pawn, _)), (x1,y1)) = True
isLegalMoveAux board (((x,y), (Rook, side)), (x1,y1)) 
    | x == x1 =
        let destination = lookup (x1,y1) board
            hasNothingInBetween = all (== Nothing) [lookup (x, iy) board | iy <- numberInBetween y y1]
        in case side of
            White -> not (isWhite destination) && hasNothingInBetween
            Black -> not (isBlack destination) && hasNothingInBetween
    | y == y1 =
        let destination = lookup (x1,y1) board
            hasNothingInBetween = all (== Nothing) [lookup (ix, y) board | ix <- numberInBetween x x1]
        in case side of
            White -> not (isWhite destination) && hasNothingInBetween
            Black -> not (isBlack destination) && hasNothingInBetween
    | otherwise = False

isLegalMoveAux board (((x,y), (Bishop, side)), (x1,y1)) =
    if abs (x1 - x) /= abs (y1 - y)
    then False
    else let destination = lookup (x1,y1) board
             inBetween = [lookup (ix, iy) board | ix <- numberInBetween x x1, iy <- numberInBetween y y1, abs (ix - x) == abs (iy - y)]
             hasNothingInBetween = all (== Nothing) inBetween
        in case side of 
            White -> not (isWhite destination) && hasNothingInBetween
            Black -> not (isBlack destination) && hasNothingInBetween


-- isLegalMoveAux board ((()))

legalMoves :: Board -> (Pos, Piece) -> [Pos]
legalMoves board ((x, y), (Pawn, side)) = undefined
legalMoves board ((x,y), (Rook, side)) = filter (\pos -> isLegalMove board (((x,y), (Rook, side)),pos)) (generateMoves ((x,y), (Rook, side)))
legalMoves board ((x,y), (Bishop, side)) = filter (\pos -> isLegalMove board (((x,y), (Bishop, side)),pos)) (generateMoves ((x,y), (Bishop, side)))

--OPTIMIZATION NOTES:
--IF A MOVE MAKES A KING VULNERABLE --> NOT LEGAL

-- look for king, if still have both king --> return Nothing, otherwise return winning side
win :: Board -> Maybe Side
win = undefined

-- you take in a board and a move, then return a new board after the change
makeMove :: Board -> Move -> Board
makeMove board (fromSquare, toPos) = 
    let updatedBoard = [(pos, piece) | (pos, piece) <- board, pos /= fst fromSquare]
    in (toPos, snd fromSquare) : updatedBoard

-- for the solver, not do yet
-- generate all legal moves for a given color on a given turn
allLegalMoves :: Board -> Side -> [((Pos, Piece, [Pos]))]
allLegalMoves = undefined
