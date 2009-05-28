module MoveValidator where

import qualified Data.Map as Map

import GameSetup
import Position
import ChessPiece

isValidMove :: Position -> Position -> GameSetup -> Bool
isValidMove p1 p2 setup 
    | not (onBoard p1 || onBoard p2) = False
    | otherwise = isValidMoveHelper cp1 cp2 p1 d
        where cp1 = pieceAt p1 setup
              cp2 = pieceAt p2 setup
              d = distance p1 p2

isValidMoveHelper :: Maybe ChessPiece -> Maybe ChessPiece -> Position -> Distance -> Bool
isValidMoveHelper Nothing _ _ _ = False
-- TODO: put into clearPath
isValidMoveHelper (Just (ChessPiece _ White)) (Just (ChessPiece _ White)) _ _ = False
isValidMoveHelper (Just (ChessPiece Pawn White)) (Just _) (0, 1) = False
isValidMoveHelper (Just (ChessPiece Pawn White)) (Just _) (0, 2) = False
isValidMoveHelper _ _ _ = True

isEl :: Distance -> Bool
isEl d
    | (abs $ fst d) == 1 && (abs $ snd d) == 3 = True
    | (abs $ fst d) == 3 && (abs $ snd d) == 1 = True
    | otherwise = False

isDiagnal :: Distance -> Bool
isDiagnal d = fst d == snd d

isLinear :: Distance -> Bool
isLinear (1, _) = True
isLinear (_, 1) = True
isLinear (_, _) = False


