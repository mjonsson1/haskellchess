import Chess 
import Data.Char
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe
import Text.XHtml (rows)


--issues that may arise with these test cases will be with turn counter.
{- 
_ _ _ _ _ _ _ _ 
_ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ 
_ _ _ _ _ _ _ _ 
_ _ _ _ _ _ _ _ 
_ _ _ _ _ _ _ _ 
_ _ _ _ _ _ _ _ 
_ _ _ _ _ _ _ _ 
-}
blankBoard = []

{-
_ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ 
_ _ _ _ _ _ _ _ 
_ _ _ _ _ _ _ _ 
_ _ _ _ _ _ _ _ 
_ _ _ _ _ _ _ _
_ _ _ _ K _ _ _
-}
gameOverOneKing = []

{-
White mate in 1
r _ b q k b _ r 
p p _ n p p p p 
_ _ p _ _ n _ _ 
_ _ _ _ _ _ _ _ 
_ _ _ P N _ _ _ 
_ _ _ _ _ _ _ _ 
P P P _ Q P P P 
R _ B _ K B N R 
-}

mateInOne = []

mateInTwo = []

mateInFour = []

pinnedKing = []