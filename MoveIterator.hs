module MoveIterator where
import Data.Char
import Data.List

data Kind = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Show)
data Color = White | Black deriving (Show)
data Piece = Piece Kind Color
type Position = (Char, Int)
type Move = (Int, Int)

rank :: Position -> Int
rank (_, rank) = rank

--assume pawn is always white, do conversion elsewhere
iterateMoves :: Piece -> Position -> [Position]
iterateMoves (Piece kind color) position = case kind of
    Pawn -> let pawnIncrements = case color of
                    White | (rank position) == 2 -> [(0, 1), (0, 2)]
                          | otherwise -> [(0, 1)]
                    Black | (rank position) == 7 -> [(0, -1), (0, -2)]
                          | otherwise -> [(0,-1)]
            in applySquareIncrements position pawnIncrements
    Knight -> applySquareIncrements position knightIncrements
    Bishop -> iterateSquareIncrements position diagnalIncrements
    Rook -> iterateSquareIncrements position horizontalIncrements
    Queen -> iterateSquareIncrements position allIncrements
    King -> applySquareIncrements position allIncrements


iterateSquareIncrements, applySquareIncrements :: Position -> [Move] -> [Position]
iterateSquareIncrements position increments = concatMap (iterateSquares position) increments
    where iterateSquares position increment = takeWhile onBoard $ 
                                              tail $ 
                                              iterate (`add` increment) position  

applySquareIncrements position increments = filter onBoard $ map (add position) increments

diagnalIncrements, verticalIncrements, horizontalIncrements, knightIncrements :: [Move]
diagnalIncrements = [(x,y) | x <- [1,-1], y <- [1,-1]]
verticalIncrements = [(0,1), (0,-1)]
horizontalIncrements = [(1,0), (-1,0)]
knightIncrements = [(x,y) | x <- [-1,-2,1,2], y <- [-1,-2,1,2], abs x /= abs y]
allIncrements = horizontalIncrements ++ verticalIncrements ++ diagnalIncrements


add :: Position -> Move -> Position
add (file, rank) (fm, rm) = (chr ((ord file) + fm), rank + rm)

-- between operator
(>=<) :: (Ord a) => a -> (a,a) -> Bool
n >=< (l, u) = n >= l && n <= u

onBoard :: Position -> Bool
onBoard (file, rank) = file >=< ('a','h') && rank >=< (1,8)




