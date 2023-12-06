import Chess
import Data.Char
import Data.Maybe (isNothing)
import InputOutput
import Solver
import System.Environment
import System.Console.GetOpt
import Text.Read
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
  let (rating, move) = moveEstimate game (determineDynamicDepth game)
  in case move of 
    Just solver_move -> 
      let newGame = makeMove game solver_move
      in case newGame of 
        Just newGame1 -> newGame1
        Nothing -> error "Generated solver move but could not make it on the board."
    Nothing -> error "Could not generate best move."


data Flag = Help | Winner | Depth (Maybe Int) | TwoPlayer | Verbose | Move String | Interactive deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [ Option ['h'] ["help"] (NoArg Help) "Print usage information and exit."
  , Option ['v'] ["verbose"] (NoArg Verbose) "Prints out a visual representation of the information as well as vital information on the quality of the move and the state of the game."
  , Option ['w'] ["winner"] (NoArg Winner) "Print out winning move with absolute solver."
  , Option ['d'] ["depth"] (ReqArg (\num -> Depth (readMaybe num)) "<num>") "Add a specific depth parameter to AI Estimate solver. Defaults to 4."
  , Option ['t'] ["twoplayer"] (NoArg TwoPlayer) "Play two player game locally."
  --, Option ['m'] ["move"] (ReqArg Move (\move -> Move (readMove move))  "<move>") "Input a move in format ((x1,y1), (Side Piece)(x2,y2)) and print out the result"
  , Option ['i'] ["interactive"] (NoArg Interactive) "Interactive play with AI."
  ]

main :: IO ()
main = do
  args <- getArgs
  let (flags, inputs, errors) = getOpt Permute options args
  let fname = if null inputs then "./txtcases/initialBoard.txt" else head inputs
  game@(_, _, _) <- loadGame fname
  if Help `elem` flags
    then putStrLn $ usageInfo "Chess [options] [file]" options
    else if TwoPlayer `elem` flags
      then startTwoPlayer game
      else if Interactive `elem` flags
            then startInteractiveMode game
            else
              processFlags game flags


processFlags :: Game -> [Flag] -> IO ()
processFlags game flags
  | Winner `elem` flags = putPerfectMove game flags
{- | Move `elem` flags = putMoveResult game flags-}
  | otherwise = putGoodMove game flags

putPerfectMove :: Game -> [Flag] -> IO ()
putPerfectMove game flags =
    case getDepthFromFlags flags of
        Nothing -> error "Depth flag cannot be combined with the Winner flag"
        Just depth -> putStrLn $ showPrettyMove2 (bestMove game) 
putGoodMove :: Game -> [Flag] -> IO ()
putGoodMove game flags =
    case getDepthFromFlags flags of
        Nothing -> error "Invalid depth input"
        Just depth -> putStrLn $ showPrettyMove (snd (moveEstimate game depth))

getDepthFromFlags :: [Flag] -> Maybe Int
getDepthFromFlags [] = Just 5
getDepthFromFlags (Depth depth : _) = depth
getDepthFromFlags (_ : rest) = getDepthFromFlags rest


startTwoPlayer :: Game -> IO ()
startTwoPlayer game = startTurn game False

startInteractiveMode :: Game -> IO ()
startInteractiveMode game = startTurn game True


