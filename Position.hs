module Position where

import Data.Char

data Position = Position {file :: Char, rank :: Int} 
                deriving (Eq, Ord, Show)

type Distance = (Int, Int)

fileOrd :: Position -> Int
fileOrd p = ord $ file p

distance :: Position -> Position -> Distance
distance p1 p2 = ((fileOrd p1) - (fileOrd p2), (rank p1) - (rank p2))

onBoard :: Position -> Bool
onBoard p 
    | file p < 'a' || file p > 'h' = False
    | rank p < 1 || rank p > 8 = False
    | otherwise = True

positionsByRank :: Int -> [Position]
positionsByRank rank = [Position f r | (f, r) <- zip ['a'..'h'] (repeat rank) ]

allPositions :: [(Int, [Position])]
allPositions = [(r, positionsByRank r) | r <- [1..8]]

