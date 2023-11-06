
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
{- in pseudo code
    board = update (prev board)
-}

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
        letterLine = concat $ intersperse "   " ["", "H", "G", "F", "E", "D", "C", "B", "A"]
    in unlines (intersperse separator rows) ++ "\n " ++ letterLine

-- Converts a board to a string from white's perspective (white is on the bottom).
showBoardWhite :: Board -> String
showBoardWhite board = 
    let rows = [showRow y White board | y <- [8, 7..1]]
        separator = "   " ++ replicate (31) '-'
        letterLine = concat $ intersperse "   " ["", "A", "B", "C", "D", "E", "F", "G", "H"]
    in unlines (intersperse separator rows) ++ "\n " ++ letterLine

--Notes from Marco

-- I think we need to specify which turn we're on to ensure the right board generates automatically, main just generates White for now.
turn :: Int -> Side
turn n = if (odd n) then White else Black


--Marco

generatePawnMoves :: (Pos,Piece) -> Board -> [Pos]
generatePawnMoves ((x,y), (Pawn, side)) board = 
    let pushOnce = case side of
            White -> [(x, y+1)|y+1 <= 8, isNothing (lookup (x,y+1) board)]
            Black -> [(x, y - 1) | y - 1 >= 1, isNothing (lookup (x, y - 1) board)]
        pushTwice = case side of
            White -> [(x,y+2)| y+2 <= 8, isNothing (lookup (x,y-2) board), y==2]
            Black -> [(x,y-2) | y-2 >= 1, isNothing (lookup(x,y-2) board), y==7]
        captures = case side of
            White -> [(x+dx,y+1) | dx <- [-1,1], x+dx >= 1, x+dx <= 8, isSomething (lookup (x+dx,y+1) board)]
            Black -> [(x+dx,y-1)| dx <- [-1,1], x+dx >= 1, x+dx <= 8, isSomething (lookup (x+dx,y-1) board)]
        in pushOnce ++ pushTwice ++ captures

generateBishopMoves :: (Pos,Piece) -> Board -> [Pos]
generateBishopMoves ((x,y), (Bishop, side)) board = 
        let downLeftDiagonalMoves = case side of
                Black -> [(x-i,y-i)|i <- [0..8], x-i <= 8, x-i>= 1, y-i<= 8, y-i >= 1]
                White -> [(x-i,y-i)|i <- [0..8], x-i <= 8, x-i>= 1, y-i<= 8, y-i >= 1]
            downRightDiagonalMoves = case side of
                Black -> [(x+i,y-i)|i <- [0..8], x+i <= 8, x+i>= 1, y-i<= 8, y-i >= 1]
                White -> [(x+i,y-i)|i <- [0..8], x+i <= 8, x+i>= 1, y-i<= 8, y-i >= 1]
            upLeftDiagonalMoves = case side of
                Black -> [(x-i,y+i)|i <- [0..8], x-i <= 8, x-i>= 1, y+i<= 8, y+i >= 1]
                White -> [(x-i,y+i)|i <- [0..8], x-i <= 8, x-i>= 1, y+i<= 8, y+i >= 1]
            upRightDiagonalMoves = case side of
                Black -> [(x+i,y+i)|i <- [0..8], x+i <= 8, x+i>= 1, y+i<= 8, y+i >= 1]
                White -> [(x+i,y+i)|i <- [0..8], x+i <= 8, x+i>= 1, y+i<= 8, y+i >= 1]
            in nub (downLeftDiagonalMoves ++ downRightDiagonalMoves ++ upLeftDiagonalMoves ++ upRightDiagonalMoves)
--Need to add optimization for checking for collisions

generateRookMoves :: (Pos,Piece) -> Board -> [Pos]
generateRookMoves ((x,y), (Rook, side)) board = 
        let verticalMoves = case side of
                Black -> [(x,y+i)|i<-[-8..8], x <= 8, x>= 1, y+i <= 8, y+i>= 1]
                White -> [(x,y+i)|i<-[-8..8], x <= 8, x>= 1, y+i <= 8, y+i>= 1]
            horizontalMoves = case side of
                Black -> [(x+i,y)|i<-[-8..8], x+i <= 8, x+i >= 1, y <= 8, y>= 1]
                White -> [(x+i,y)|i<-[-8..8], x+i <= 8, x+i >= 1, y <= 8, y>= 1]
        in nub (verticalMoves ++horizontalMoves)
--check if empty

--lookup takes in a coordinate and outputs a piece tuple, 
--need to define a function to do the reverse, so that we can find the input position values to generate movess
isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = False
--Check if not empty
isSomething :: Maybe a -> Bool
isSomething (Just _) = True
isSomething Nothing = False

--LEVI
main =
    putStrLn $ showBoard initialBoard White

--NATE
-- given a piece and a position, return ALL POSSIBLE positions to move that piece (respecting bounds).
generateMoves :: (Pos, Piece) -> [Pos]
generateMoves ((x,y), (Rook, _)) = undefined
generateMoves ((x,y), (Pawn, Black)) = undefined

--KHOI
-- filter all possible moves for a given piece into only legal moves 
legalMoves :: Board -> (Pos, Piece) -> [Pos]
legalMoves board ((x, y), piece) = undefined

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


