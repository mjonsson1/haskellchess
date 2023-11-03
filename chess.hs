import Data.Char
import Data.List
import Text.XHtml (rows)

import Data.List

data PieceType = Pawn | King | Bishop | Knight | Queen | Rook deriving (Show, Eq)
data Side = Black | White deriving (Show, Eq)

type Pos = (Int, Int) -- (x, y)

--to navigate the squares around a piece
type Square = (Pos, Piece)

--to keep track of each piece
type Piece = (PieceType, Side)
type Board = [Square]

--maybe isCheck, isCheckMate
type Move = (Square, Pos)

-- (currentBoard, currentSideTurn)
type Game = (Board, Side)

initialBoard :: Board 
initialBoard = [((1, 1), Rook White),
                ((2, 1), Knight White),
                ((3, 1), Bishop White),
                ((4, 1), Queen White),
                ((5, 1), King White),
                ((6, 1), Bishop White),
                ((7, 1), Knight White),
                ((8, 1), Rook White),
                ((1, 2), Pawn White),
                ((2, 2), Pawn White),
                ((3, 2), Pawn White),
                ((4, 2), Pawn White),
                ((5, 2), Pawn White),
                ((6, 2), Pawn White),
                ((7, 2), Pawn White),
                ((8, 2), Pawn White),
                ((1, 8), Rook Black),
                ((2, 8), Knight Black),
                ((3, 8), Bishop Black),
                ((4, 8), Queen Black),
                ((5, 8), King Black),
                ((6, 8), Bishop Black),
                ((7, 8), Knight Black),
                ((8, 8), Rook Black),
                ((1, 7), Pawn Black),
                ((2, 7), Pawn Black),
                ((3, 7), Pawn Black),
                ((4, 7), Pawn Black),
                ((5, 7), Pawn Black),
                ((6, 7), Pawn Black),
                ((7, 7), Pawn Black),
                ((8, 7), Pawn Black)
                ]


instance Show Piece where
    show (Rook Black) = " ♖ "
    show (Knight Black) = " ♘ "
    show (Bishop Black) = " ♗ "
    show (Queen Black) = " ♕ "
    show (King Black) = " ♔ "
    show (Pawn Black) = " ♙ "
    show (Rook White) = " ♜ "
    show (Knight White) = " ♞ "
    show (Bishop White) = " ♝ "
    show (Queen White) = " ♛ "
    show (King White) = " ♚ "
    show (Pawn White) = " ♟ "
    show (NullP White) = "   "


-- Function to create a single row of the chessboard with rooks
createRow :: Int-> Int -> Board -> String
createRow sz y board = concat $ intersperse "|" [show (getPiece (x,y) board)| x <- [1..sz]]

getPiece :: Position -> Board -> Piece
getPiece pos board = case lookup pos board of
                        Just piece -> piece
                        Nothing -> ChessPiece NullP White

-- Function to create the chessboard with n rows and n columns
createChessBoardBlack :: Int-> Int -> Board -> String
createChessBoardBlack sz n board = unlines $ intersperse separator rows
    where
        rows = [createRow sz y board | y <- [1..n]]
        separator = replicate (sz * 4 - 1) '-'
createChessBoardWhite :: Int-> Int -> Board -> String
createChessBoardWhite sz n board = unlines $ intersperse separator rows
    where
        rows = [createRow sz y board | y <- [n,n-1..1]]
        separator = replicate (sz * 4 - 1) '-'
main = putStrLn $ createChessBoardBlack 8 8 initialBoard


-- given a piece and a position, return ALL POSSIBLE positions to move that piece.
generateMoves :: (Position, Piece) -> [Position]
generateMoves = undefined


-- filter all possible moves for a given piece into only legal moves 
legalMoves :: Board -> (Position, Piece) -> [Position]
legalMoves (x, y) piece = undefined


-- given a board and a king position, determine if the king has any legal moves.
isCheckMate :: Board -> Position -> Bool
isCheckMate = undefined


-- generate all legal moves for a given color on a given turn
allLegalMoves :: Board -> Color -> [((Position, Piece, [Position]))]
allLegalMoves = undefined


-- from all legal moves, make a move for a color and return a new board.
makeMove :: Board -> Color -> Board
makeMove = undefined

