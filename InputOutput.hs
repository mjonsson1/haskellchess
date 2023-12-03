module InputOutput where

import Chess
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe
import Solver
import Debug.Trace
import System.IO

--                                        SHOWING BOARD

showPiecePretty :: Piece -> String
showPiecePretty (Rook, Black) = " ♖ "
showPiecePretty (Knight, Black) = " ♘ "
showPiecePretty (Bishop, Black) = " ♗ "
showPiecePretty (Queen, Black) = " ♕ "
showPiecePretty (King, Black) = " ♔ "
showPiecePretty (Pawn, Black) = " ♙ "
showPiecePretty (Rook, White) = " ♜ "
showPiecePretty (Knight, White) = " ♞ "
showPiecePretty (Bishop, White) = " ♝ "
showPiecePretty (Queen, White) = " ♛ "
showPiecePretty (King, White) = " ♚ "
showPiecePretty (Pawn, White) = " ♟ "

-- Returns a string representing specified row on the board
showRow :: Int -> Side -> Board -> String
showRow y White board = concat $ [show y, "  "] ++ intersperse "|" [lookupPiece (x, y) board | x <- [1 .. 8]]
showRow y Black board = concat $ [show y, "  "] ++ intersperse "|" [lookupPiece (x, y) board | x <- [8, 7 .. 1]]

-- Returns a string of the piece symbol at the given location on the board
lookupPiece :: Pos -> Board -> String
lookupPiece pos board = case lookup pos board of
  Just piece -> showPiecePretty piece
  Nothing -> "   "

-- Converts a board to a string when printed to output.
-- If not printed to output, newlines and pieces are not displayed properly.
showBoard :: Board -> String
showBoard board =
  let rows = [showRow y White board | y <- [8, 7 .. 1]]
      separator = "   " ++ replicate (31) '-'
      coordinateLine = intercalate "   " ["", "a", "b", "c", "d", "e", "f", "g", "h"]
   in unlines (intersperse separator rows) ++ "\n " ++ coordinateLine

--                                        TEXT REPRESENTATION
--                                        STRING TO BOARD

stringToPiece :: String -> Maybe Piece
stringToPiece s =
  case s of
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
    _ -> Nothing

buildSquare :: ((Int, Int), Maybe Piece) -> Maybe Square
buildSquare ((colNum, rowNum), Just piece) = Just ((colNum, rowNum), piece)
buildSquare ((colNum, rowNum), Nothing) = Nothing

rowToBoard :: String -> Int -> Maybe [Square]
rowToBoard rowString rowNum =
  let pieces = splitOn " " rowString
   in sequence [buildSquare ((colNum, rowNum), stringToPiece piece) | (colNum, piece) <- zip [1 ..] pieces, piece /= "_"]

-- parses our simple board notation and converts to a Board
stringToBoard :: String -> Maybe Board
stringToBoard boardString =
  let rows = lines boardString
   in fmap concat $ sequence [rowToBoard row rowNum | (rowNum, row) <- zip [8, 7 .. 1] rows]

--                                        BOARD TO STRING

pieceToString :: Maybe Piece -> String
pieceToString piece =
  case piece of
    Nothing -> "_"
    Just (Rook, Black) -> "r"
    Just (Knight, Black) -> "n"
    Just (Bishop, Black) -> "b"
    Just (Queen, Black) -> "q"
    Just (King, Black) -> "k"
    Just (Pawn, Black) -> "p"
    Just (Rook, White) -> "R"
    Just (Knight, White) -> "N"
    Just (Bishop, White) -> "B"
    Just (Queen, White) -> "Q"
    Just (King, White) -> "K"
    Just (Pawn, White) -> "P"

rowToString :: Board -> Int -> String
rowToString board rowNum = unwords [pieceToString (lookup (colNum, rowNum) board) | colNum <- [1 .. 8]]

boardToString :: Board -> String
boardToString board = unlines [rowToString (filter (\((column, row), piece) -> row == x) board) x | x <- [8, 7 .. 1]]

--                                        GAME TO STRING
readGame :: String -> Game
readGame gameString =
  let (headerRow : boardRows) = lines gameString
      header = splitOn " " headerRow
      side = case (head header) of
        "W" -> White
        "B" -> Black
      turnsLeft = read (last header)
      board = stringToBoard (intercalate "\n" boardRows)
   in 
    if board == Nothing
      then error "Failed to read board!"
      else 
        let Just newBoard = board
        in (newBoard, side, turnsLeft)

showGame :: Game -> String
showGame (board, side, turnsLeft) =
  let sideString = case side of
        White -> "W"
        Black -> "B"
   in sideString ++ " " ++ (show turnsLeft) ++ "\n" ++ boardToString board

writeGame :: Game -> FilePath -> IO ()
writeGame game filepath = do
  let gameString = showGame game
  handle <- openFile filepath WriteMode
  hPutStr handle gameString
  hClose handle
  return ()

loadGame :: FilePath -> IO Game
loadGame filepath = do
  gameContent <- (readFile filepath)
  let game = readGame gameContent
  return game

putBestMove :: Game -> IO ()
putBestMove game = do
  let bm = bestMove game
      (board, side, turn) = makeUnSafeMove game bm
  putStrLn $ showBoard board

{- main :: IO ()
main =
  do
    args <- getArgs
    let fname = head args
    game@(initialboard, _, _) <- loadGame fname
    putStrLn "initial board: "
    putStrLn $ showBoard initialboard
    putStrLn "new board: "
    putBestMove game
-}