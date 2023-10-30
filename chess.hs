import Data.Char
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

offSet = [(0,1), (1,0), (0,-1), (-1,0)]

-- allMoves :: Square -> [Move]
-- allMoves (pos, (Rook, side)) =  
--     let aux :: Int -> 