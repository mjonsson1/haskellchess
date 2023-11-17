module Chess where

import Data.Char
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe
import Text.XHtml (rows)

--                                               DATATYPES

data PieceType = Pawn | King | Bishop | Knight | Queen | Rook deriving (Show, Eq)

data Side = Black | White deriving (Show, Eq)

data Winner = WinningSide Side | Tie deriving (Show, Eq)

type Pos = (Int, Int) -- (x, y)

type Piece = (PieceType, Side)

type Square = (Pos, Piece)

type Board = [Square]

type Move = (Square, Pos)

type Game = (Board, Side, Int)

--                                               BOARDS
initialBoard :: Board
initialBoard =
  [ ((1, 1), (Rook, White)),
    ((2, 1), (Knight, White)),
    ((3, 1), (Bishop, White)),
    ((4, 1), (Queen, White)),
    ((5, 1), (King, White)),
    ((6, 1), (Bishop, White)),
    ((7, 1), (Knight, White)),
    ((8, 1), (Rook, White))
  ]
    ++ [((x, 2), (Pawn, White)) | x <- [1 .. 8]]
    ++ [((x, 7), (Pawn, Black)) | x <- [1 .. 8]]
    ++ [ ((1, 8), (Rook, Black)),
         ((2, 8), (Knight, Black)),
         ((3, 8), (Bishop, Black)),
         ((4, 8), (Queen, Black)),
         ((5, 8), (King, Black)),
         ((6, 8), (Bishop, Black)),
         ((7, 8), (Knight, Black)),
         ((8, 8), (Rook, Black))
       ]

emptyBoard :: Board
emptyBoard = []

--                                                 HELPER FUNCTIONS
inBound :: (Int, Int) -> Bool
inBound (x, y) = (x >= 1 && x <= 8) && (y >= 1 && y <= 8)

recurCheckPath :: Board -> Side -> Pos -> (Int, Int) -> [Pos]
recurCheckPath board side point dir -- ix, iy are offsets -> a num in [-1..1]
  | not $ inBound newPoint = []
  | otherwise =
      case lookup newPoint board of
        Nothing -> newPoint : recurCheckPath board side newPoint dir
        Just (piece, color) -> [newPoint | color == opponent side]
  where
    newPoint = offset point dir

offset :: Pos -> Pos -> Pos
offset (x, y) (ix, iy) = (x + ix, y + iy)

opponent :: Side -> Side
opponent White = Black
opponent Black = White

isAlly :: Side -> Maybe Piece -> Bool
isAlly side maybePiece =
  case maybePiece of
    Nothing -> False
    Just (_, pieceSide) -> pieceSide /= opponent side

isOpponent :: Side -> Maybe Piece -> Bool
isOpponent side maybePiece =
  case maybePiece of
    Nothing -> False
    Just (_, pieceSide) -> pieceSide == opponent side

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
  let queenDir = [(-1, 0), (0, 1), (0, -1), (1, 0), (-1, 1), (1, 1), (-1, -1), (1, -1)]
      allpos = concat [recurCheckPath board side (x, y) dir | dir <- queenDir]
   in [(square, pos) | pos <- allpos]
-- KNIGHT
allLegalMoves square@((x, y), (Knight, side)) board =
  let allSquares = [(x + i, y + j) | i <- [-2 .. 2], j <- [-2 .. 2], abs (i * j) == 2, inBound (x + i, y + j)]
      legalSquares = filter (\pos -> not (isAlly side (lookup pos board))) allSquares
   in [(square, pos) | pos <- legalSquares]
-- KING
allLegalMoves square@((x, y), (King, side)) board =
  let oneMove = [-1, 0, 1]
      surrounding = [(x + i, y + j) | i <- oneMove, j <- oneMove, inBound (x + i, y + j)]
      surroundingWithoutStart = delete (x, y) surrounding
      legalSquares = filter (\pos -> not (isAlly side (lookup pos board))) surroundingWithoutStart
   in [(square, pos) | pos <- legalSquares]
-- PAWN
-- TODO: delete bound check for Pawn, bc if a y is at the edge: turn into queen
allLegalMoves square@((x, y), (Pawn, White)) board =
  let pushOnce = [(x, y + 1) | inBound (x, y + 1), isNothing (lookup (x, y + 1) board)]
      pushTwice = [(x, y + 2) | y == 2, isNothing (lookup (x, y + 2) board), isNothing (lookup (x, y + 1) board)]
      captures = [(x + dx, y + 1) | dx <- [-1, 1], inBound (x + dx, y + 1), isOpponent White (lookup (x + dx, y + 1) board)]
      allPos = pushOnce ++ pushTwice ++ captures
   in [(square, pos) | pos <- allPos]
allLegalMoves square@((x, y), (Pawn, Black)) board =
  let pushOnce = [(x, y - 1) | inBound (x, y - 1), isNothing (lookup (x, y - 1) board)]
      pushTwice = [(x, y - 2) | y == 7, isNothing (lookup (x, y - 2) board), isNothing (lookup (x, y - 1) board)]
      captures = [(x + dx, y - 1) | dx <- [-1, 1], inBound (x + dx, y - 1), isOpponent Black (lookup (x + dx, y - 1) board)]
      allPos = pushOnce ++ pushTwice ++ captures
   in [(square, pos) | pos <- allPos]

-- look for king, if still have both king --> return Nothing, otherwise return winning side
win :: Board -> Maybe Side
win board =
  let tmp = filter (\((_, _), (pType, _)) -> pType == King) board
   in if length tmp /= 2 then Just (snd (snd (head tmp))) else Nothing

-- NOTE: the fromSquare@ is Fogarty's suggestion, do not delete
-- you take in a board and a move, then return a new board after the change
makeMove :: Board -> Move -> Maybe Board
makeMove board move@(fromSquare@(startPos, movingPiece), toPos)
  | move `notElem` (allLegalMoves fromSquare board) = Nothing
  | otherwise =
      let updatedBoard = [(pos, piece) | (pos, piece) <- board, pos /= startPos, pos /= toPos]
       in Just ((toPos, movingPiece) : updatedBoard)

-- making a move without considering whether it is legal
makeUnSafeMove :: Board -> Move -> Board
makeUnSafeMove board move@(fromSquare@(startPos, movingPiece), toPos) =
  let updatedBoard = [(pos, piece) | (pos, piece) <- board, pos /= startPos, pos /= toPos]
   in (toPos, movingPiece) : updatedBoard