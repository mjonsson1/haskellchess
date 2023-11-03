import Data.Char
import Data.List
import Text.XHtml (rows)

{-
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

startBoard :: Board = [
    -- white pieces
    ((1, 1), (Rook, White)),
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
    -- black pieces, 
    ((1, 8), (Rook, Black)),
    ((2, 8), (Knight, Black)),
    ((3, 8), (Bishop, Black)),
    ((4, 8), (Queen, Black)),
    ((5, 8), (King, Black)),
    ((6, 8), (Bishop, Black)),
    ((7, 8), (Knight, Black)),
    ((8, 8), (Rook, Black)),
    ((1, 7), (Pawn, Black)),
    ((2, 7), (Pawn, Black)),
    ((3, 7), (Pawn, Black)),
    ((4, 7), (Pawn, Black)),
    ((5, 7), (Pawn, Black)),
    ((6, 7), (Pawn, Black)),
    ((7, 7), (Pawn, Black)),
    ((8, 7), (Pawn, Black))
    ]




{-

boardGen:: Board -> [[String]]
boardGen board =
    let piece = ""
        boardCoordinates = [((x,y), piece)| x<- [1..8], y<-[1..8]]
        in [(pieceCoord,piece)| pieceCoord <- boardCoordinates, piece `elem`]
    where   aux::((Int, Int),Piece) -> Maybe Piece
            aux (c,p) = if c `elem` board then


startBoardInit = [((x,y), piece)| x<- [1..8], y<-[1..8]]


piece <- (if (x,y) `elem` startBoard, then snd show startBoard else "")
offSet = [(0,1), (1,0), (0,-1), (-1,0)]

printBoard :: Board -> [[ String ]]
fullSetOfCoords = [((x,y), piece)| x<- [1..8], y<-[1..8]]
    where aux :: Pos -> Board -> Piece
          aux (x,y) [(x,y),(p,team)] = if (x,y) `elem` map fst board then snd [(p,team)]

-}

createRow :: Int -> String
createRow n = concat $ intersperse "|" $ (replicate n  " " ++ showPiece Pe (Rook,Black) ++ " ")

createChessBoard :: Int -> String
createChessBoard n = unlines $ intersperse separator rows
            where   rows = replicate n (createRow n)
                    separator = replicate (n*4 - 1) '-'
showPiece :: Piece -> String
showPiece Piece (Rook, Black) = "R, B"

main :: IO ()
main = putStrLn $ createChessBoard 10

-- allMoves :: Square -> [Move]
-- allMoves (pos, (Rook, side)) =  
--     let aux :: Int -> 
-}
import Data.List

data Color = Black | White deriving Eq
data PieceType = Pawn | King | Bishop | Knight | Queen | Rook | NullP deriving (Show, Eq)
data Piece = ChessPiece PieceType Color
type Position = (Int,Int)
type Board = [(Position, Piece)]

initialBoard :: Board 
initialBoard = [((1, 1), ChessPiece Rook White),
                ((2, 1), ChessPiece Knight White),
                ((3, 1), ChessPiece Bishop White),
                ((4, 1), ChessPiece Queen White),
                ((5, 1), ChessPiece King White),
                ((6, 1), ChessPiece Bishop White),
                ((7, 1), ChessPiece Knight White),
                ((8, 1), ChessPiece Rook White),
                ((1, 2), ChessPiece Pawn White),
                ((2, 2), ChessPiece Pawn White),
                ((3, 2), ChessPiece Pawn White),
                ((4, 2), ChessPiece Pawn White),
                ((5, 2), ChessPiece Pawn White),
                ((6, 2), ChessPiece Pawn White),
                ((7, 2), ChessPiece Pawn White),
                ((8, 2), ChessPiece Pawn White),
                ((1, 8), ChessPiece Rook Black),
                ((2, 8), ChessPiece Knight Black),
                ((3, 8), ChessPiece Bishop Black),
                ((4, 8), ChessPiece Queen Black),
                ((5, 8), ChessPiece King Black),
                ((6, 8), ChessPiece Bishop Black),
                ((7, 8), ChessPiece Knight Black),
                ((8, 8), ChessPiece Rook Black),
                ((1, 7), ChessPiece Pawn Black),
                ((2, 7), ChessPiece Pawn Black),
                ((3, 7), ChessPiece Pawn Black),
                ((4, 7), ChessPiece Pawn Black),
                ((5, 7), ChessPiece Pawn Black),
                ((6, 7), ChessPiece Pawn Black),
                ((7, 7), ChessPiece Pawn Black),
                ((8, 7), ChessPiece Pawn Black)
                ]
instance Show Piece where
    show (ChessPiece Rook Black) = " ♖ "
    show (ChessPiece Knight Black) = " ♘ "
    show (ChessPiece Bishop Black) = " ♗ "
    show (ChessPiece Queen Black) = " ♕ "
    show (ChessPiece King Black) = " ♔ "
    show (ChessPiece Pawn Black) = " ♙ "
    show (ChessPiece Rook White) = " ♜ "
    show (ChessPiece Knight White) = " ♞ "
    show (ChessPiece Bishop White) = " ♝ "
    show (ChessPiece Queen White) = " ♛ "
    show (ChessPiece King White) = " ♚ "
    show (ChessPiece Pawn White) = " ♟ "
    show (ChessPiece NullP White) = "   "

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

