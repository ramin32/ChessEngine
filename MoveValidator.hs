module MoveValidator where

import qualified Data.Map as Map

import GameSetup
import Position
import ChessPiece

isValidMove :: Position -> Position -> GameSetup -> Bool
isValidMove p1 p2 setup
    | cp1 == Nothing = False
    | not $ isPathClear p1 p2 setup = False
    | otherwise = isValidMoveHelper cp1 d
        where cp1 = pieceAt p1 setup
              cp2 = pieceAt p2 setup
              d = distance p1 p2

isPathClear :: Position -> Position -> GameSetup -> Bool
isPathClear p1 p2 setup = True

isValidMoveHelper :: Maybe ChessPiece -> Distance -> Bool
isValidMoveHelper cp1 d = True
