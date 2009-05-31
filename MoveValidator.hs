module MoveValidator where

import qualified Data.Map as Map
import Data.Maybe


import Position
import ChessPiece
import GameSetup

isValidMove :: Position -> Position -> GameSetup -> Bool
isValidMove p1 p2 setup 
    | not (onBoard p1 || onBoard p2) = False
    | otherwise = isValidMoveHelper cp1 cp2 p1 d
        where cp1 = Map.lookup p1 setup
              cp2 = Map.lookup p2 setup
              d = distance p1 p2

isValidMoveHelper :: Maybe ChessPiece -> Maybe ChessPiece -> Position -> Distance -> Bool
isValidMoveHelper Nothing _ _ _ = False

-- Capture same color
isValidMoveHelper (Just (ChessPiece _ White)) (Just (ChessPiece _ White)) _ _ = False
isValidMoveHelper (Just (ChessPiece _ Black)) (Just (ChessPiece _ Black)) _ _ = False

-- Pawn validation
isValidMoveHelper (Just (ChessPiece Pawn White)) Nothing (Position _ 2) (0, 1) = True
isValidMoveHelper (Just (ChessPiece Pawn White)) Nothing (Position _ 2) (0, 2) = True

isValidMoveHelper (Just (ChessPiece Pawn Black)) Nothing (Position _ 7) (0, 1) = True
isValidMoveHelper (Just (ChessPiece Pawn Black)) Nothing (Position _ 7) (0, 2) = True

isPathClear :: [Position] -> GameSetup -> Bool
isPathClear ps setup = and mappedToNothing
    where innerPositions = (init . tail) ps
          lookedUpPositions = map (`Map.lookup` setup) innerPositions
          mappedToNothing = map (== Nothing) lookedUpPositions 

executeMove :: Position -> Position -> GameSetup -> GameSetup
executeMove p1 p2 setup 
    | isValidMove p1 p2 setup = Map.insert p2 (fromJust $ Map.lookup p1 setup) (Map.delete p1 setup)
    | otherwise = setup

