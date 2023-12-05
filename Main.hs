import Chess
import Data.Char
import Data.Maybe (isNothing)
import InputOutput
import Solver
import System.Environment
import System.Console.GetOpt

toLowerString :: String -> String
toLowerString = map toLower

letters :: [Char]
letters = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']

numStr :: [Char]
numStr = ['1', '2', '3', '4', '5', '6', '7', '8']

readPos :: (String, String) -> Maybe (Pos, Pos)
readPos ([letter1, num1], [letter2, num2]) =
  let letterToNum = zip letters [1 .. 8]
      numStrToNum = zip numStr [1 .. 8]
   in do
        -- if one of these return a nothing, for ex: p9, then return nothing
        x1 <- lookup letter1 letterToNum
        y1 <- lookup num1 numStrToNum
        x2 <- lookup letter2 letterToNum
        y2 <- lookup num2 numStrToNum
        return ((x1, y1), (x2, y2))
readPos _ = Nothing

readMove :: String -> Game -> Maybe Move
readMove line (board, side, turn) =
  let split :: [String]
      split = filter (not . null) (words line)
   in -- check if input is two strings
      case split of
        [first, sec] ->
          case readPos (toLowerString first, toLowerString sec) of
            -- if the input format is not "e2 e4" --> return Nothing
            Nothing -> Nothing
            Just ((x, y), (x1, y1)) ->
              let maybeP = lookup (x, y) board
               in case maybeP of
                    -- if there is no piece at the start
                    Nothing -> Nothing
                    Just p -> Just (((x, y), p), (x1, y1))
        -- if the input format is not two strings
        _ -> Nothing

recurReadInput :: Game -> Bool -> IO ()
recurReadInput game isInteractive = do
  moveStr <- getLine
  case readMove moveStr game of
    Nothing -> do
      putStrLn "Invalid input. Please enter a valid input (in format: d2 d4): "
      recurReadInput game isInteractive
    Just move@(((x, y), (pType, side)), (x1, y1)) ->
      case makeMove game move of
        Just gameAfterPlayerMove -> do 
          if isInteractive then do
            putStrLn $ showPrettyGame gameAfterPlayerMove
            putStrLn "Calculating solver move..."
            let gameAfterSolverMove = makeSolverMove gameAfterPlayerMove
            startTurn gameAfterSolverMove isInteractive
          else startTurn gameAfterPlayerMove isInteractive
        Nothing -> do
          putStrLn "This is not a valid move, try again: "
          recurReadInput game isInteractive

-- print the current turn's board and recursively ask for input
startTurn :: Game -> Bool -> IO ()
startTurn game@(board, sideOfPlayer, turnNum) isInteractive = do
  putStrLn ""
  putStrLn $ showPrettyGame game
  case whoHasWon (board, sideOfPlayer, turnNum) of
    Nothing -> do
      putStrLn ("Enter move for " ++ (toLowerString (show sideOfPlayer)) ++ " (in format: d2 d4): ")
      recurReadInput (board, sideOfPlayer, turnNum) isInteractive
    Just end -> case end of
      WinningSide side -> putStrLn (show side ++ " is the winner!")
      Tie -> putStrLn "It's a tie!"


determineDynamicDepth :: Game -> Int
determineDynamicDepth game = 
  case (length (gameMoveAssociation game)) of
    n | n < 10 -> 6
    n | n >= 10 && n < 20 -> 5
    n | n >= 20 && n < 30 -> 4
    n | n > 30 -> 4


makeSolverMove :: Game -> Game
makeSolverMove game = 
  let (rating, move) = whoMightWin game (determineDynamicDepth game)
  in case move of 
    Just solver_move -> 
      let newGame = makeMove game solver_move
      in case newGame of 
        Just newGame1 -> newGame1
        Nothing -> error "Generated solver move but could not make it on the board."
    Nothing -> error "Could not generate best move."


-- TODO FIX FLAGS

-- data Flag = Help | Quick | Number String | Start String | TwoPlayer deriving (Eq, Show)
-- options :: [OptDescr  Flag]
-- options = [ Option ['h'] ["help"] (NoArg Help) "Print usage information and exit."
--             Option ['w'] ["winner"] (ReqArg) "Print out winning move with absolute solver."
--             ,Option ['t'] ["twoplayer"] (NoArg TwoPlayer) "Play two player game."
--                 {- "Start at fortune <num>. Defaults to value based on name, or 1 if quick mode." -}
--           ]


-- main :: IO ()
-- main = 
--   do args <- getArgs
--      let (flags, inputs, errors) =  getOpt Permute options args
--      let fname = if null inputs then "./txtcases/initialBoard.txt" else head inputs
--      game@(_, _, _) <- loadGame fname
--      if Help `elem` flags
--        then putStrLn $ usageInfo "Fortunes [options] [file]" options
--        else
--           if TwoPlayer `elem` flags
--           then
--               startTwoPlayer game
--             else
--               error "Not a valid Flag"


startTwoPlayer :: Game -> IO ()
startTwoPlayer game = startTurn game False

startInteractiveMode :: Game -> IO ()
startInteractiveMode game = startTurn game True

-- This main function just runs a two player game from the start
{-
main :: IO ()
main = do
  startTurn (initialBoard, White, 30)
-}

-- This main allows you to load in specific tester case boards to play in 2 player format
{-
main :: IO ()
main =
  do
    args <- getArgs
    let fname = head args
    game@(initialboard, _, _) <- loadGame fname
    startTurn game
-}
-- This main allows you to print all next game states
{-
printAllBoard :: [Board] -> IO ()
printAllBoard [b] = do putStrLn $ showBoard b
printAllBoard (b : bs) = do
  putStrLn $ showBoard b
  printAllBoard bs

main :: IO ()
main = do
  let newGames = allNextGame (initialBoard, White, 50)
  let allBoards = [board | (board, _, _) <- newGames]
  printAllBoard allBoards
-}

-- This  main allows you to feed a tester board game state to check AI behavior

main :: IO ()
main =
  do
    -- args <- getArgs
    -- let fname = head args
    game <- loadGame "./txtcases/initialBoard.txt"
    putStrLn "Initial board: "
    putStrLn $ showPrettyGame game
    startInteractiveMode game

{-
main :: IO ()
main = 
    do
      args <- getArgs
      let fname = head args
      game <- loadGame fname
      putStrLn $ show (whoMightWin game 5)

-}