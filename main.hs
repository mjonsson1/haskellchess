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


readMove :: String -> Maybe Move
readMove line =  
    let split :: [String]
        split = filter (not . null) (words line)
    --check if input is two strings 
    in case split of 
        [first, sec] -> readPos (toLowerString first, toLowerString sec)
        
        _            -> Nothing   

recurReadInput :: Side -> Board -> IO()
recurReadInput turn board = do
    move <- getLine
    case readMove move of 
        Nothing -> do
            putStrLn "Invalid input. Please enter a valid input (in format: d2 d4): "
            recurReadInput turn board
        Just ((startX, startY), (endX, endY))  -> do
            case getActualPiece (startX, startY) board of
                Nothing -> do
                    putStrLn "Moving from an empty square, try again: "
                    recurReadInput turn board
                --this is the piece that is being moved
                Just (pType, side) ->
                    if side /= turn
                    then do
                        putStrLn "Can not move opponent piece, try again: "
                        recurReadInput turn board
                    else do
                        if (isLegalMove ((startX, startY), (endX, endY)) board)
                        then do
                            putStrLn "insert move function here"
                            let newBoard = board
                            if turn == White
                            then startTurn Black newBoard
                            else startTurn White newBoard
                        else do
                            putStrLn "This is not a valid move, try again: "
                            recurReadInput turn board

--print the current turn's board and recursively ask for input
startTurn :: Side -> Board -> IO()
startTurn turn board = do 
    putStrLn $ showBoard board turn
    putStrLn ("Enter move for " ++ (toLowerString (show turn)) ++ " (in format: d2 d4): ")
    recurReadInput turn board
    

main :: IO()
main = do
    startTurn White initialBoard