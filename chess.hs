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
type Move = (Square, Pos)

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

--                                        SHOWING BOARD

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


-- Returns a string representing specified row on the board
showRow :: Int -> Side -> Board -> String
showRow y White board = concat $ [show y, "  "] ++ intersperse "|" [getPiece (x,y) board | x <- [1..8]]
showRow y Black board = concat $ [show y, "  "] ++ intersperse "|" [getPiece (x,y) board | x <- [8, 7..1]]
-- Returns a string of the piece symbol at the given location on the board
getPiece :: Pos -> Board -> String
getPiece pos board = case lookup pos board of
                        Just piece -> showPiece piece
                        Nothing -> "   "

-- Converts a board to a string when printed to output. 
-- If not printed to output, newlines and pieces are not displayed properly. 
showBoard :: Board -> Side -> String
showBoard board Black = showBoardBlack board
showBoard board White = showBoardWhite board

-- Converts a board to a string from black's perspective (black is on the bottom).
showBoardBlack :: Board -> String
showBoardBlack board = 
    let rows = [showRow y Black board | y <- [1..8]]
        separator = "   " ++ replicate (31) '-'
        coordinateLine = concat $ intersperse "   " ["", "h", "g", "f", "e", "d", "c", "b", "a"]
    in unlines (intersperse separator rows) ++ "\n " ++ coordinateLine

-- Converts a board to a string from white's perspective (white is on the bottom).
showBoardWhite :: Board -> String
showBoardWhite board = 
    let rows = [showRow y White board | y <- [8, 7..1]]
        separator = "   " ++ replicate (31) '-'
        coordinateLine = concat $ intersperse "   " ["", "a", "b", "c", "d", "e", "f", "g", "h"]
    in unlines (intersperse separator rows) ++ "\n " ++ coordinateLine


fenToBoard :: String -> Board
fenToBoard = undefined


boardToFen :: Board -> String
boardToFen = undefined
