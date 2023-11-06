import Data.Char
import Data.List
import Text.XHtml (rows)

import Data.List

data PieceType = Pawn | King | Bishop | Knight | Queen | Rook deriving (Show, Eq)
data Side = Black | White deriving (Show, Eq)

type Pos = (Int, Int) -- (x, y)

--to keep track of each piece
type Piece = (PieceType, Side)

--to navigate the squares around a piece
type Square = (Pos, Piece)
type Board = [Square]

--maybe isCheck, isCheckMate
type Move = (Square, Pos)

-- (currentBoard, currentSideTurn)
type Game = (Board, Side)

initialBoard :: Board 
-- change to Board type 
initialBoard = [((1, 1), (Rook, White)),
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

--AIDEN: ALL THE SHOW BOARD
-- Function to create a single row of the chessboard with rooks
createRow :: Int-> Int -> Board -> String
createRow sz y board = concat $ intersperse "|" [show (getPiece (x,y) board)| x <- [1..sz]]

--TODO: Change
getPiece :: Position -> Board -> Piece
getPiece pos board = case lookup pos board of
                        Just piece -> piece
                        Nothing -> ChessPiece NullP White

--TODO: Fix, function takes in board, the current side that's playing and print the respective board
showBoard :: Board -> Side -> String

-- Function to create the chessboard with n rows and n columns
showBoardBlack :: Board -> String
showBoardBlack sz n board = unlines $ intersperse separator rows
    where
        rows = [createRow sz y board | y <- [1..n]]
        separator = replicate (sz * 4 - 1) '-'
showBoardWhite :: Board -> String
showBoardWhite sz n board = unlines $ intersperse separator rows
    where
        rows = [createRow sz y board | y <- [n,n-1..1]]
        separator = replicate (sz * 4 - 1) '-'




--LEVI
main = putStrLn $ createChessBoardBlack 8 8 initialBoard

--NATE
-- given a piece and a position, return ALL POSSIBLE positions to move that piece (respecting bounds).
generateMoves :: (Position, Piece) -> [Position]
generateMoves ((x,y), (Rook, _)) = undefined
generateMoves ((x,y), (Pawn, Black))

--KHOI
-- filter all possible moves for a given piece into only legal moves 
legalMoves :: Board -> (Position, Piece) -> [Position]
legalMoves (x, y) piece = undefined

--OPTIMIZATION NOTES:
--IF A MOVE MAKES A KING VULNERABLE --> NOT LEGAL


-- look for king, if still have both king --> return Nothing, otherwise return winning side
win :: Board -> Maybe Side
win = undefined



-- for the solver, not do yet
-- generate all legal moves for a given color on a given turn
allLegalMoves :: Board -> Color -> [((Position, Piece, [Position]))]
allLegalMoves = undefined

--MAHAD
-- you take in a board and a move, then return a new board after the change
makeMove :: Board -> Move -> Board
makeMove = undefined


