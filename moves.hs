module Moves where
import Data.List
import Chess

--                                                 HELPER FUNCTIONS
inBound :: Int -> Bool
inBound x = x >= 1 && x <= 8
--keep going in one direction until hit something with the same side or after capture an enemy
recurCheckPath :: Board -> Side -> (Int, Int) -> (Int, Int) -> [Pos]
recurCheckPath board side (currentX, currentY) (ix, iy) --ix, iy are offsets -> a num in [-1..1]
  | not (inBound (currentX + ix) && inBound (currentY + iy)) = []
  | otherwise =
      let nextPos = (currentX + ix, currentY + iy)
          nextMaybePiece = lookup nextPos board
        in if isNothing nextMaybePiece
            then nextPos : recurCheckPath board side nextPos (ix, iy)
            else case side of
              White ->
                if isWhite nextMaybePiece
                  then []
                  else [nextPos]
              Black ->
                if isBlack nextMaybePiece
                  then []
                  else [nextPos]

--                                            GENERATING ALL LEGAL MOVES
allLegalMoves :: Square -> Board -> [Move]
-- BISHOP
allLegalMoves square@((x, y), (Bishop, side)) board =
  let offsetbishop = [(-1, 1), (1, 1), (-1, -1), (1, -1)]
      allpos = concat [recurCheckPath board side (x, y) offset | offset <- offsetbishop]
  in [(square, pos) | pos <- allpos]

  
  -- let upLeft = recurCheckPath board side (x,y) (-1, 1)
  --     upRight = recurCheckPath board side (x,y) (1, 1)
  --     downLeft = recurCheckPath board side (x,y) (-1, -1)
  --     downRight = recurCheckPath board side (x,y) (1, -1)
  --     allPos = upLeft ++ upRight ++ downLeft ++ downRight
  --  in [(square, pos) | pos <- allPos]
-- ROOK
allLegalMoves square@((x, y), (Rook, side)) board =
  let offsetrook = [(-1, 0), (0, 1), (0, -1), (1, 0)]
      allpos = concat [recurCheckPath board side (x, y) offset | offset <- offsetrook]
  in [(square, pos) | pos <- allpos]
-- QUEEN
allLegalMoves square@((x, y), (Queen, side)) board =
  let offsetqueen = [(-1, 0), (0, 1), (0, -1), (1, 0),(-1, 1), (1, 1), (-1, -1), (1, -1)]
      allpos = concat [recurCheckPath board side (x, y) offset | offset <- offsetqueen]
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
-- KING
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

-- OPTIMIZATION NOTES:
-- IF A MOVE MAKES A KING VULNERABLE --> NOT LEGAL

-- look for king, if still have both king --> return Nothing, otherwise return winning side
win :: Board -> Maybe Side
win board = 
  let   tmp = filter (\((_,_),(pType, _)) -> pType == King) board in
        if length tmp /= 2 then Just (snd (snd (head tmp))) else Nothing


-- you take in a board and a move, then return a new board after the change
makeMove :: Board -> Move -> Maybe Board
makeMove board move@(fromSquare, toPos)
  | move `notElem` (allLegalMoves fromSquare board) = Nothing
  | otherwise =
      let updatedBoard = [(pos, piece) | (pos, piece) <- board, pos /= fst fromSquare, pos /= toPos]
       in Just ((toPos, snd fromSquare) : updatedBoard)
