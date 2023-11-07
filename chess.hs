
module Chess where
    
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
type Move = (Pos, Pos)

-- (currentBoard, currentSideTurn)
type Game = (Board, Side)

initialBoard :: Board 
-- change to Board type 
initialBoard = [((1, 1), (Rook, White)),
                ((2, 1), (Knight, White)),
                ((3, 1), (Bishop, White)),
                ((4, 1), (Queen, White)),
                ((5, 1), (King, White)),
                ((6, 1), (Bishop, White)),
                ((7, 1), (Knight, White)),
                ((8, 1), (Rook, White)),
                
                ((1, 2), (Pawn, White)),
                ((2, 2), (Pawn, White)),
                ((3, 2), (Pawn, White)),
                ((4, 2), (Pawn, White)),
                ((5, 2), (Pawn, White)),
                ((6, 2), (Pawn, White)),
                ((7, 2), (Pawn, White)),
                ((8, 2), (Pawn, White)),

                ((1, 7), (Pawn, Black)),
                ((2, 7), (Pawn, Black)),
                ((3, 7), (Pawn, Black)),
                ((4, 7), (Pawn, Black)),
                ((5, 7), (Pawn, Black)),
                ((6, 7), (Pawn, Black)),
                ((7, 7), (Pawn, Black)),
                ((8, 7), (Pawn, Black)),
                
                ((1, 8), (Rook, Black)),
                ((2, 8), (Knight, Black)),
                ((3, 8), (Bishop, Black)),
                ((4, 8), (Queen, Black)),
                ((5, 8), (King, Black)),
                ((6, 8), (Bishop, Black)),
                ((7, 8), (Knight, Black)),
                ((8, 8), (Rook, Black))
                ]

showPiece :: Piece -> String
showPiece (Rook, Black) = " ♖ "
showPiece (Knight, Black) = " ♘ "
showPiece (Bishop, Black) = " ♗ "
showPiece (Queen, Black) = " ♕ "
showPiece (King, Black) = " ♔ "
showPiece (Pawn, Black) = " ♙ "
showPiece (Rook, White) = " ♜ "
showPiece (Knight, White) = " ♞ "
showPiece (Bishop, White) = " ♝ "
showPiece (Queen, White) = " ♛ "
showPiece (King, White) = " ♚ "
showPiece (Pawn, White) = " ♟ "


--AIDEN: ALL THE SHOW BOARD
-- Function to create a single row of the chessboard with rooks
showRow :: Int -> Side -> Board -> String
showRow y White board = concat $ intersperse "|" [getPiece (x,y) board | x <- [1..8]]
showRow y Black board = concat $ intersperse "|" [getPiece (x,y) board | x <- [8, 7..1]]

getActualPiece :: Pos -> Board -> Maybe Piece
getActualPiece pos board = lookup pos board

--TODO: Change
getPiece :: Pos -> Board -> String
getPiece pos board = case lookup pos board of
                        Just piece -> showPiece piece
                        Nothing -> "   "

--TODO: Fix, function takes in board, the current side that's playing and print the respective board
showBoard :: Board -> Side -> String
showBoard board Black = showBoardBlack board
showBoard board White = showBoardWhite board

-- Function to create the chessboard with n rows and n columns
showBoardBlack :: Board -> String
showBoardBlack board = 
    let rows = [showRow y Black board | y <- [1..8]]
        separator = replicate (31) '-'
    in unlines $ intersperse separator rows

showBoardWhite :: Board -> String
showBoardWhite board = 
    let rows = [showRow y White board | y <- [8, 7..1]]
        separator = replicate (31) '-'
    in unlines $ intersperse separator rows


--LEVI
-- main = putStrLn $ showBoard initialBoard White

--NATE
-- given a piece and a position, return ALL POSSIBLE positions to move that piece (respecting bounds).
generateMoves :: (Pos, Piece) -> [Pos]
generateMoves ((x,y), (Rook, _)) = undefined
generateMoves ((x,y), (Pawn, Black)) = undefined

--KHOI
-- filter all possible moves for a given piece into only legal moves 
legalMoves :: Board -> (Pos, Piece) -> [Pos]
legalMoves board ((x, y), piece) = undefined

isLegalMove :: Move -> Board -> Bool
isLegalMove ((startX, startY), (endX, endY)) board = 
    case getActualPiece (startX, startY) board of
        Nothing -> False
            -- p :: Piece
        Just p -> 
            let pLegalMoves :: [Pos]
                pLegalMoves = legalMoves board ((startX, startY), p) 
            in (endX, endY) `elem` pLegalMoves

--OPTIMIZATION NOTES:
--IF A MOVE MAKES A KING VULNERABLE --> NOT LEGAL


-- look for king, if still have both king --> return Nothing, otherwise return winning side
win :: Board -> Maybe Side
win = undefined


-- for the solver, not do yet
-- generate all legal moves for a given color on a given turn
allLegalMoves :: Board -> Side -> [((Pos, Piece, [Pos]))]
allLegalMoves = undefined

--MAHAD
-- you take in a board and a move, then return a new board after the change
makeMove :: Board -> Move -> Board
makeMove = undefined


