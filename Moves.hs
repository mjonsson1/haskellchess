module Moves where
import Data.List
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
  let bishopDir = [(-1, 1), (1, 1), (-1, -1), (1, -1)]
      allpos = concat [recurCheckPath board side (x, y) dir | dir <- bishopDir]
  in [(square, pos) | pos <- allpos]
-- ROOK
allLegalMoves square@((x, y), (Rook, side)) board =
  let rookDir = [(-1, 0), (0, 1), (0, -1), (1, 0)]
      allpos = concat [recurCheckPath board side (x, y) dir | dir <- rookDir]
  in [(square, pos) | pos <- allpos]
-- QUEEN
allLegalMoves square@((x, y), (Queen, side)) board =
  let queenDir = [(-1, 0), (0, 1), (0, -1), (1, 0),(-1, 1), (1, 1), (-1, -1), (1, -1)]
      allpos = concat [recurCheckPath board side (x, y) dir | dir <- queenDir]
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

color :: Maybe Piece -> Maybe Side
color piece = fmap snd piece

offset :: Pos -> Pos -> Pos
offset (x,y) (ix, iy) = (x+ix,y+iy)

opponent :: Side -> Side
opponent White = Black
opponent Black = White


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
