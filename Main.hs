import Chess
import Data.Maybe (isNothing)
import Distribution.Verbosity (verbose)
import InputOutput
import Solver
import System.Console.GetOpt
import System.Environment
import Text.Read
import Text.XHtml (start)

-- Read user's input and switch turn for two-player mode or let AI make move for interactive mode
recurReadInput :: Game -> Bool -> Int -> IO ()
recurReadInput game isInteractive depth = do
  moveStr <- getLine
  case readMove moveStr game of
    Nothing -> do
      putStrLn "Invalid input. Please enter a valid input (in format: d2 d4): "
      recurReadInput game isInteractive depth
    Just move@(((x, y), (pType, side)), (x1, y1)) ->
      -- Checking if the user's move is valid
      case makeMove game move of
        Nothing -> do
          putStrLn "This is not a valid move, try again: "
          recurReadInput game isInteractive depth
        Just gameAfterPlayerMove -> do
          if isInteractive
            then do
              putStrLn $ showPrettyGame gameAfterPlayerMove
              putStrLn $ "Calculating solver move..." ++ " Depth: "++ show depth
              let gameAfterSolverMove = makeSolverMove gameAfterPlayerMove depth
              startTurn gameAfterSolverMove isInteractive depth
            else startTurn gameAfterPlayerMove isInteractive depth


-- print the current turn's board and recursively ask for input
startTurn :: Game -> Bool -> Int -> IO ()
startTurn game@(board, sideOfPlayer, turnNum) isInteractive depth = do
  putStrLn ""
  putStrLn $ showPrettyGame game
  case whoHasWon (board, sideOfPlayer, turnNum) of
    Nothing -> do
      putStrLn $ ("Enter move for " ++ (toLowerString (show sideOfPlayer)) ++ " (in format: d2 d4): ")
      recurReadInput (board, sideOfPlayer, turnNum) isInteractive depth
    Just end -> case end of
      WinningSide side -> putStrLn (show side ++ " is the winner!")
      Tie -> putStrLn "It's a tie!"

startTwoPlayer :: Game -> IO ()
startTwoPlayer game = startTurn game False 0

--                                                    AI INTERACTIVE

-- MUST BE COMBINABLE WITH -D
determineDynamicDepth :: Game -> Int
determineDynamicDepth game =
  case (length (gameMoveAssociation game)) of
    n | n < 10 -> 8
    n | n >= 10 && n < 20 -> 7
    n | n >= 20 && n < 30 -> 5
    n | n >= 30 -> 5

makeSolverMove :: Game -> Int -> Game
makeSolverMove game depth =
  if depth == 0 then let (rating, move) = moveEstimate game (determineDynamicDepth game)
                        in case move of
                              Just solver_move -> makeUnSafeMove game solver_move
                              Nothing -> error "Could not generate best move."
                else  let (rating, move) = moveEstimate game depth
                        in case move of
                              Just solver_move -> makeUnSafeMove game solver_move
                              Nothing -> error "Could not generate best move."

startInteractiveMode :: Game -> Int -> IO ()
startInteractiveMode game depth = startTurn game True depth



--                                                  FLAGS HANDLING

data Flag = Help | Winner | Depth (Maybe Int) | TwoPlayer | Verbose | MoveInput String | Interactive deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [ Option ['h'] ["help"] (NoArg Help) "Print usage information and exit.",
    Option ['v'] ["verbose"] (NoArg Verbose) "Prints out a visual representation of the information as well as a rating for the game.",
    Option ['w'] ["winner"] (NoArg Winner) "Print out winning move with absolute solver.",
    Option ['d'] ["depth"] (ReqArg (\num -> Depth (readMaybe num)) "<num>") "Add a specific depth parameter to AI Estimate solver. Defaults to dynamic depth calculation.",
    Option ['t'] ["twoplayer"] (NoArg TwoPlayer) "Play two player game locally.",
    Option ['m'] ["move"] (ReqArg MoveInput "move") "Input a move in <d2 d4> and print out the result ",
    Option ['i'] ["interactive"] (NoArg Interactive) "Interactive play with AI. Can be paired with the depth flag. Defaults to dynamic depth calculation. "
  ]

main :: IO ()
main = do
  args <- getArgs
  let (flags, inputs, errors) = getOpt Permute options args
  let fname = if null inputs then "./txtcases/initialBoard.txt" else head inputs
  game@(_, _, _) <- loadGame fname
  if Help `elem` flags
    then putStrLn $ usageInfo "Chess [options] [file]" options
    else
      if TwoPlayer `elem` flags
        then startTwoPlayer game
        else
          if Interactive `elem` flags
            then
              case getDepthFromFlags flags game of
              Nothing -> startInteractiveMode game (determineDynamicDepth game)
              Just depth ->  startInteractiveMode game depth
            else
              if Verbose `elem` flags
                then processFlagsVerbose game flags
                else processFlags game flags

processFlags :: Game -> [Flag] -> IO ()
processFlags game flags
  | Winner `elem` flags = putPerfectMove game flags
  | any isMoveInput flags = handleMoveInput game flags
  | otherwise = putGoodMove game flags

processFlagsVerbose :: Game -> [Flag] -> IO ()
processFlagsVerbose game flags
  | Winner `elem` flags = putPerfectMoveVerbose game flags
  | any isMoveInput flags = handleMoveInputVerbose game flags
  | otherwise = putGoodMoveVerbose game flags

isMoveInput :: Flag -> Bool
isMoveInput (MoveInput _) = True
isMoveInput _ = False

--                                              WINNER FLAG
putPerfectMove :: Game -> [Flag] -> IO ()
putPerfectMove game flags =
  case getDepthFromFlags flags game of
    Nothing -> error "Depth flag cannot be combined with the Winner flag"
    Just depth -> putBestMove game

putPerfectMoveVerbose :: Game -> [Flag] -> IO ()
putPerfectMoveVerbose game flags =
  case getDepthFromFlags flags game of
    Nothing -> error "Depth flag cannot be combined with the Winner flag"
    Just depth -> putBestMoveVerbose game

--                                               MOVE FLAG

handleMoveInput :: Game -> [Flag] -> IO ()
handleMoveInput _ [] = putStrLn "No move input provided."
handleMoveInput game (MoveInput moveStr : rest) = do
  putStrLn $ "Received move input: " ++ moveStr
  case readMove moveStr game of
    Nothing -> putStrLn "Invalid move input."
    Just move -> do
      case makeMove game move of
        Nothing -> putStrLn $ (showPrettyMove move) ++ "is not a valid move."
        Just newGame -> do
          putStrLn $ "Parsed move: " ++ showPrettyMove move
          putStrLn "Board after:"
          putStrLn $ showPrettyGame newGame
-- Continue processing other flags in 'rest' if necessary
handleMoveInput game (_ : rest) = handleMoveInput game rest -- Skip other flag types

handleMoveInputVerbose :: Game -> [Flag] -> IO ()
handleMoveInputVerbose _ [] = putStrLn "No move input provided."
handleMoveInputVerbose game (MoveInput moveStr : rest) = do
  putStrLn $ "Received move input: " ++ moveStr
  case readMove moveStr game of
    Nothing -> putStrLn "Invalid move input."
    Just move -> do
      case makeMove game move of
        Nothing -> putStrLn $ (showPrettyMove move) ++ "is not a valid move."
        Just newGame -> do
          putStrLn $ "Parsed move: " ++ showPrettyMove move
          putStrLn "Board after:"
          putStrLn $ showPrettyGame newGame
          verboseRatingPrint newGame
-- Continue processing other flags in 'rest' if necessary
handleMoveInputVerbose game (_ : rest) = handleMoveInputVerbose game rest -- Skip other flag types

--                                                 NO FLAG
putGoodMove :: Game -> [Flag] -> IO ()
putGoodMove game flags =
  case getDepthFromFlags flags game of
    Nothing -> error "Invalid depth input"
    Just depth -> putStrLn $ "You should make move: " ++  showPrettyMaybeMove (snd (moveEstimate game depth))

putGoodMoveVerbose :: Game -> [Flag] -> IO ()
putGoodMoveVerbose game flags =
  case getDepthFromFlags flags game of
    Nothing -> error "Invalid depth input"
    Just depth -> do
      case snd (moveEstimate game depth) of
        Nothing -> do
          putStrLn $ "Game is already won"
        Just em -> do
          putStrLn $ "You should make move: " ++ showPrettyMove em
          let newGame = makeUnSafeMove game em
          putStrLn "Initial board:"
          putStrLn $ showPrettyGame game
          putStrLn "Board after:"
          putStrLn $ showPrettyGame newGame
          verboseRatingPrint newGame

getDepthFromFlags :: [Flag] -> Game -> Maybe Int
getDepthFromFlags [] game = Just (determineDynamicDepth game)
getDepthFromFlags (Depth depth : _) game = depth
getDepthFromFlags (_ : rest) game = getDepthFromFlags rest game


