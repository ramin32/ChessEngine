module Board where

import Piece

type OccupiedSquare = (Position, Piece)
data Board = Board { whiteSquares :: [OccupiedSquare]
                   , blackSquares :: [OccupiedSquare]
                   } deriving (Show)


firstRank = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
allFiles = ['A'..'H']
pawns = repeat Pawn

generateRank :: Int -> [Piece] -> [OccupiedSquare]
generateRank rank pieces = 
    [((file, rank), piece ) | (file, piece) <- zip allFiles pieces]

whiteOpponent, blackOpponent :: [OccupiedSquare]
whiteOpponent = (generateRank 1 firstRank) ++ (generateRank 2 pawns)
blackOpponent = (generateRank 7 pawns) ++ (generateRank 8 firstRank)

newBoard :: Board
newBoard = Board whiteOpponent blackOpponent 
