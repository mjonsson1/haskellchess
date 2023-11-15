module Chess where

import Data.Char
import Data.List
import Data.List.Split (splitOn)
import Text.XHtml (rows)
import Data.Maybe

data PieceType = Pawn | King | Bishop | Knight | Queen | Rook deriving (Show, Eq)

data Side = Black | White deriving (Show, Eq)

type Pos = (Int, Int) -- (x, y)

type Piece = (PieceType, Side)

type Square = (Pos, Piece)

type Board = [Square]

type Move = (Square, Pos)

type Winner = Maybe Side

initialBoard :: Board
-- change to Board type
initialBoard =
  [ ((1, 1), (Rook, White)),
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
  
emptyBoard::Board
emptyBoard = []
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
showRow y White board = concat $ [show y, "  "] ++ intersperse "|" [lookupPiece (x,y) board | x <- [1..8]]
showRow y Black board = concat $ [show y, "  "] ++ intersperse "|" [lookupPiece (x,y) board | x <- [8, 7..1]]

-- Returns a string of the piece symbol at the given location on the board
lookupPiece :: Pos -> Board -> String
lookupPiece pos board = case lookup pos board of
                        Just piece -> showPiece piece
                        Nothing -> "   "

-- Converts a board to a string when printed to output.
-- If not printed to output, newlines and pieces are not displayed properly.
showBoard :: Board -> String
showBoard board = 
    let rows = [showRow y White board | y <- [8, 7 .. 1]]
        separator = "   " ++ replicate (31) '-'
        coordinateLine = concat $ intersperse "   " ["", "a", "b", "c", "d", "e", "f", "g", "h"]
    in unlines (intersperse separator rows) ++ "\n " ++ coordinateLine


--                                        TEXT REPRESENTATION
--                                        STRING TO BOARD

pieceFromLetter :: String -> Maybe Piece
pieceFromLetter s = 
    case s of 
        "_" -> Nothing
        "" -> Nothing
        "r" -> Just (Rook, Black)
        "n" -> Just (Knight, Black)
        "b" -> Just (Bishop, Black)
        "q" -> Just (Queen, Black)
        "k" -> Just (King, Black)
        "p" -> Just (Pawn, Black)
        "R" -> Just (Rook, White)
        "N" -> Just (Knight, White)
        "B" -> Just (Bishop, White)
        "Q" -> Just (Queen, White)
        "K" -> Just (King, White)
        "P" -> Just (Pawn, White)

buildSquare :: ((Int, Int), Maybe Piece) -> Maybe Square
buildSquare ((colNum, rowNum), piece) = 
    case piece of 
        Nothing -> Nothing
        Just p -> Just ((colNum, rowNum), p)

rowToBoard :: String -> Int -> [Square]
rowToBoard rowString rowNum = 
    let pieces = splitOn " " rowString
    in catMaybes [buildSquare ((colNum, rowNum), pieceFromLetter piece) | (colNum, piece) <- zip [1..] pieces]

-- parses our simple board notation and converts to a Board
stringToBoard :: String -> Board
stringToBoard boardString = 
    let rows = splitOn " \n" boardString
    in concat [rowToBoard row rowNum | (rowNum, row) <- zip [8, 7..1] rows]


--                                        BOARD TO STRING

pieceToString :: Maybe Piece -> String
pieceToString piece = 
    case piece of
        Nothing -> "_ "
        Just (Rook, Black) -> "r "
        Just (Knight, Black) -> "n "
        Just (Bishop, Black) -> "b "
        Just (Queen, Black) -> "q "
        Just (King, Black) -> "k "
        Just (Pawn, Black) -> "p "
        Just (Rook, White) -> "R "
        Just (Knight, White) -> "N "
        Just (Bishop, White) -> "B "
        Just (Queen, White) -> "Q "
        Just (King, White) -> "K "
        Just (Pawn, White) -> "P "

rowToString :: Board -> Int -> String
rowToString board rowNum = (concat [pieceToString (lookup (colNum, rowNum) board) | colNum <- [1..8]]) ++ "\n"

boardToString :: Board -> String
boardToString board = concat [rowToString (filter (\((column, row), piece) -> row == x) board) x | x <- [8, 7..1]]
