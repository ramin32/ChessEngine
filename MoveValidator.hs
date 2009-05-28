module MoveValidator where

import qualified Data.Map as Map

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

-- Pawn validation
isValidMoveHelper (Just (ChessPiece Pawn White)) Nothing (Position _ 2) (0, 1) = True
isValidMoveHelper (Just (ChessPiece Pawn White)) Nothing (Position _ 2) (0, 2) = True

isValidMoveHelper (Just (ChessPiece Pawn Black)) Nothing (Position _ 7) (0, 1) = True
isValidMoveHelper (Just (ChessPiece Pawn Black)) Nothing (Position _ 7) (0, 2) = True

isValidMoveHelper _ _ _ _ = True

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
isLinear (_, _) = False

isSingleMover :: Distance -> Bool
isSingleMover d = (abs $ fst d) < 2 && (abs $ snd d) < 2

executeMove :: Position -> Position -> GameSetup -> GameSetup
executeMove p1 p2 setup 
    | isValidMove p1 p2 setup = Map.delete p1 setup
    | otherwise = setup

