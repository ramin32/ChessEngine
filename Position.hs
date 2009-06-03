module Position where

import Data.Char

data Position = Position {file :: Char, rank :: Int} 
                deriving (Eq, Ord, Show)

type Distance = (Int, Int)

fileOrd :: Position -> Int
fileOrd p = ord $ file p

distance :: Position -> Position -> Distance
distance p1 p2 = ((fileOrd p2) - (fileOrd p1), (rank p2) - (rank p1))

onBoard :: Position -> Bool
onBoard p 
    | file p < 'a' || file p > 'h' = False
    | rank p < 1 || rank p > 8 = False
    | otherwise = True

positionsByRank :: Int -> [Position]
positionsByRank r = [Position f r | f <- ['a'..'h']]

allPositions :: [(Int, [Position])]
allPositions = [(r, positionsByRank r) | r <- [1..8]]

positions :: Position -> Distance -> [Position]
positions p1 (0, 0) = p1 : []
positions p1 d = p1 : positions (Position (chr (f + xInc)) (r + yInc)) (fstD - xInc, sndD - yInc) 
    where f = fileOrd p1
          r = rank p1
          fstD = fst d
          sndD = snd d
          xInc = signum fstD
          yInc = signum sndD

isEl :: Distance -> Bool
isEl d
    | (abs $ fst d) == 1 && (abs $ snd d) == 3 = True
    | (abs $ fst d) == 3 && (abs $ snd d) == 1 = True
    | otherwise = False

isDiagnal :: Distance -> Bool
isDiagnal d = fst d == snd d

isLinear :: Distance -> Bool
isLinear (0, _) = True
isLinear (_, 0) = True
isLinear _ = False

isLinearXorDiagnal d = isLinear d || isDiagnal d 

isSingleMover :: Distance -> Bool
isSingleMover d = (abs $ fst d) < 2 && (abs $ snd d) < 2

