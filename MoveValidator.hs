module MoveValidator where

import qualified Data.Map as Map
import Data.Maybe


import Position
import ChessPiece
import GameSetup

isValidMove :: Position -> Position -> GameSetup -> Bool
isValidMove p1 p2 setup 
    | not (onBoard p1 || onBoard p2) = False
    | oponentMatchingColors = False
    | cp1IsNotKnight && not (isPathClear p1 d setup)= False
    | otherwise = isValidMoveHelper cp1 cp2 p1 d
        where cp1 = Map.lookup p1 setup
              cp2 = Map.lookup p2 setup
              d = distance p1 p2
              oponentMatchingColors = (fmap color cp1) == (fmap color cp2)
              cp1IsNotKnight = (fmap name cp1) /= Just Knight
              

isValidMoveHelper :: Maybe ChessPiece -> Maybe ChessPiece -> Position -> Distance -> Bool
isValidMoveHelper Nothing _ _ _ = False

-- Capture same color
isValidMoveHelper (Just (ChessPiece _ White)) (Just (ChessPiece _ White)) _ _ = False
isValidMoveHelper (Just (ChessPiece _ Black)) (Just (ChessPiece _ Black)) _ _ = False

-- Pawn validation
isValidMoveHelper (Just (ChessPiece Pawn White)) Nothing (Position _ 2) (0, 1) = True
isValidMoveHelper (Just (ChessPiece Pawn White)) Nothing (Position _ 2) (0, 2) = True

isValidMoveHelper (Just (ChessPiece Pawn Black)) Nothing (Position _ 7) (0, (-1)) = True
isValidMoveHelper (Just (ChessPiece Pawn Black)) Nothing (Position _ 7) (0, (-2)) = True

isValidMoveHelper _ _ _ _ = False

isPathClear :: Position -> Distance -> GameSetup -> Bool
isPathClear p1 d setup = and innerMappedToNothing 
    where innerPositions = (init . tail) ps
          lookedUpPositions = map (`Map.lookup` setup) innerPositions
          innerMappedToNothing = map (== Nothing) lookedUpPositions 
          ps = positions p1 d

executeMove :: Position -> Position -> GameSetup -> GameSetup
executeMove p1 p2 setup  
    | isValidMove p1 p2 setup = Map.insert p2 (fromJust $ Map.lookup p1 setup) (Map.delete p1 setup)
    | otherwise = setup

