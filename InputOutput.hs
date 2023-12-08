module InputOutput where

import Chess
import Data.Char (toLower)
import Data.List (intercalate, intersperse)
import Data.List.Split (splitOn)
import Data.Maybe
import Debug.Trace
import Solver
import System.IO

--                                            PRETTY PRINTING

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

-- Returns a string of the piece symbol at the given location on the board
lookupPiece :: Pos -> Board -> String
lookupPiece pos board = case lookup pos board of
  Just piece -> showPiecePretty piece
  Nothing -> "   "

-- Returns a string representing specified row on the board
-- Has extra functionality of printing the board from white's or black's perspective
showRow :: Int -> Side -> Board -> String
showRow y White board = concat $ [show y, "  "] ++ intersperse "|" [lookupPiece (x, y) board | x <- [1 .. 8]]
showRow y Black board = concat $ [show y, "  "] ++ intersperse "|" [lookupPiece (x, y) board | x <- [8, 7 .. 1]]

-- Converts a game to a pretty output String for printing
showPrettyGame :: Game -> String
showPrettyGame (board, side, turn) =
  let rows = [showRow y White board | y <- [8, 7 .. 1]]
      separator = "   " ++ replicate (31) '-'
      coordinateLine = intercalate "   " ["", "a", "b", "c", "d", "e", "f", "g", "h"]
      prettyBoard = unlines (intersperse separator rows) ++ "\n " ++ coordinateLine
   in "Current side: " ++ (show side) ++ "\n" ++ "Number of turns left: " ++ (show turn) ++ "\n" ++ prettyBoard

intToLetter :: Int -> String
intToLetter i =
  let letterAssociation = zip [1 .. 8] ["a", "b", "c", "d", "e", "f", "g", "h"]
   in case lookup i letterAssociation of
        Just x -> x
        Nothing -> error "can't find associated letter"

-- Move must be in bound of board or result would be wrong or throw error
showPrettyMaybeMove :: Maybe Move -> String
showPrettyMaybeMove move =
  case move of
    Nothing -> "Game is over, no move to be made"
    Just (((x1, y1), (pType, side)), (x2, y2)) ->
      let start = intToLetter x1 ++ show y1
          end = intToLetter x2 ++ show y2
       in "You should make the move " ++ show side ++ " " ++ show pType ++ ": " ++ start ++ " -> " ++ end

showPrettyMove :: Move -> String
showPrettyMove (((x1, y1), (pType, side)), (x2, y2)) =
  let start = intToLetter x1 ++ show y1
      end = intToLetter x2 ++ show y2
   in "You should make the move " ++ show side ++ " " ++ show pType ++ ": " ++ start ++ " -> " ++ end

--                                        TEXT REPRESENTATION
--                                            GAME TO FEN
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

--                                            FEN TO GAME

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

-- Convert a single Fen row into a row on the Board, return Nothing if can't read
rowToBoard :: String -> Int -> Maybe [Square]
rowToBoard rowString rowNum =
  let pieces = splitOn " " rowString
   in sequence [buildSquare ((colNum, rowNum), stringToPiece piece) | (colNum, piece) <- zip [1 ..] pieces, piece /= "_"]

-- Parses our fen notation and converts to a Board
stringToBoard :: String -> Maybe Board
stringToBoard boardString =
  let rows = lines boardString
   in fmap concat $ sequence [rowToBoard row rowNum | (rowNum, row) <- zip [8, 7 .. 1] rows]

-- Parses our entire fen notation and converts to a Game
-- Throws error if can not read
readGame :: String -> Game
readGame gameString =
  let (headerRow : boardRows) = lines gameString
      header = splitOn " " headerRow
      side = case (head header) of
        "W" -> White
        "B" -> Black
      turnsLeft = read (last header)
      board = stringToBoard (intercalate "\n" boardRows)
   in if board == Nothing
        then error "Failed to read board!"
        else
          let Just newBoard = board
           in (newBoard, side, turnsLeft)

-- Loading a file into a Game
loadGame :: FilePath -> IO Game
loadGame filepath = do
  gameContent <- (readFile filepath)
  let game = readGame gameContent
  return game

--                                        OUTPUTTING SOLVER RESULT

putBestMove :: Game -> IO ()
putBestMove game = do
  let bm = bestMove game
      newGame = makeUnSafeMove game bm
  putStrLn $ "You should make the move: " ++ showPrettyMove bm

verboseRatingPrint :: Game -> IO ()
verboseRatingPrint game
  | rating > 0 = putStrLn $ ratingMessage ++ ". White is winning."
  | rating < 0 = putStrLn $ ratingMessage ++ ". Black is winning."
  | otherwise = putStrLn $ ratingMessage ++ ". The game is very close!"
  where
    rating = rateGame game
    ratingMessage = "Board rating in current state: " ++ show rating

putBestMoveVerbose :: Game -> IO ()
putBestMoveVerbose game = do
  let bm = bestMove game
      newGame = makeUnSafeMove game bm
  putStrLn "Initial board: "
  putStrLn $ showPrettyGame game
  putStrLn $ "You should make the move: " ++ showPrettyMove bm
  putStrLn "Board after "
  putStrLn $ showPrettyGame newGame
  verboseRatingPrint game

--                                         READING USER'S INPUT
toLowerString :: String -> String
toLowerString = map toLower

letters :: [Char]
letters = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']

letterToNum = zip letters [1 .. 8]

digits :: [Char]
digits = ['1', '2', '3', '4', '5', '6', '7', '8']

digitToNum = zip digits [1 .. 8]

-- Takes in a tuple of user input  like ("e2", "a2") and returns a corresponding (Pos, Pos) if valid
readPos :: (String, String) -> Maybe (Pos, Pos)
readPos ([letter1, digit1], [letter2, digit2]) =
  do
    -- if one of these return a nothing, for ex: p9, then return nothing
    x1 <- lookup letter1 letterToNum
    y1 <- lookup digit1 digitToNum
    x2 <- lookup letter2 letterToNum
    y2 <- lookup digit2 digitToNum
    return ((x1, y1), (x2, y2))
readPos _ = Nothing

readMove :: String -> Game -> Maybe Move
readMove line (board, _, _) =
  let split :: [String]
      split = filter (not . null) (words line)
   in -- check if input is two strings
      case split of
        [first, sec] ->
          case readPos (toLowerString first, toLowerString sec) of
            -- if the input format is not "e2 e4" --> return Nothing
            Nothing -> Nothing
            Just ((x1, y1), (x2, y2)) ->
              case lookup (x1, y1) board of
                -- if there is no piece at the start
                Nothing -> Nothing
                Just p -> Just (((x1, y1), p), (x2, y2))
        -- if the input format is not two strings
        _ -> Nothing