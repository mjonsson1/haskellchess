import Chess
import Data.Char (toLower)

-- TODO: check if validMoves or generateMoves eliminate the starting pos
-- TODO: check if printBoardWhite flips the letters

toLowerString :: String -> String
toLowerString = map toLower

letters = ['a','b','c','d','e','f','g','h']
numStr = ['1','2','3','4','5','6','7','8']
readPos :: (String, String) -> Maybe (Pos, Pos)
readPos ([letter1, num1], [letter2, num2]) = 
    let letterToNum = zip letters [1..8]
        numStrToNum = zip numStr [1..8]
    in 
        do
            --if one of these return a nothing, for ex: p9, then return nothing
            x1 <- lookup letter1 letterToNum
            y1 <- lookup num1 numStrToNum
            x2 <- lookup letter2 letterToNum
            y2 <- lookup num2 numStrToNum
            return ((x1,y1),(x2,y2))
readPos _ = Nothing


readMove :: String -> Board -> Maybe Move
readMove line board =  
    let split :: [String]
        split = filter (not . null) (words line)
    --check if input is two strings 
    in case split of 
        [first, sec] -> 
            case readPos (toLowerString first, toLowerString sec) of
                --if the input format is not "e2 e4" --> return Nothing
                Nothing -> Nothing
                Just ((x,y), (x1,y1)) ->
                    let maybeP = lookup (x,y) board
                    in case maybeP of
                        --if there is no piece at the start
                        Nothing -> Nothing
                        Just p -> Just (((x,y), p), (x1,y1)) 
        -- if the input format is not two strings
        _            -> Nothing   

recurReadInput :: Side -> Board -> IO()
recurReadInput turn board = do
    moveStr <- getLine
    case readMove moveStr board of 
        Nothing -> do
            putStrLn "Invalid input. Please enter a valid input (in format: d2 d4): "
            recurReadInput turn board
        Just move@(((x,y),(pType, side)),(x1,y1)) -> 
            if side /= turn
            then do
                putStrLn "Can not move opponent piece, try again: "
                recurReadInput turn board
            else do
                if isLegalMove board move
                then do
                    let newBoard = makeMove board move
                    if turn == White
                    then startTurn Black newBoard
                    else startTurn White newBoard
                else do
                    putStrLn "This is not a valid move, try again: "
                    recurReadInput turn board
                --this is the piece that is being moved

--print the current turn's board and recursively ask for input
startTurn :: Side -> Board -> IO()
startTurn turn board = do 
    putStrLn $ showBoard board turn
    putStrLn ("Enter move for " ++ (toLowerString (show turn)) ++ " (in format: d2 d4): ")
    recurReadInput turn board
    

main :: IO()
main = do
    startTurn White initialBoard