module MoveValidator where

import qualified Data.Map as Map
import Data.Maybe
import Control.Applicative

import Position
import ChessPiece
import GameSetup

isValidMove :: Color -> Position -> Position -> GameSetup -> (Bool, String)
isValidMove turn p1 p2 setup 
    | cp1DoesntExists = (False, "No piece at initial location!")
    | (fmap color cp1) /= Just turn = (False, "Wrong turn!") 
    | not (onBoard p1 || onBoard p2) = (False, "Move out of bounds!")
    | oponentMatchingColors = (False, "You're the opponent!")  
    | cp1IsNotKnight && not (isPathClear p1 d setup) = (False, "Path not clear")
    | otherwise = isValidMoveHelper cp1 cp2 p1 d
        where cp1 = Map.lookup p1 setup
              cp2 = Map.lookup p2 setup
              d = distance p1 p2
              oponentMatchingColors = (fmap color cp1) == (fmap color cp2)
              cp1DoesntExists = cp1 == Nothing
              cp1IsNotKnight = (fmap name cp1) /= Just Knight
              

isValidMoveHelper :: Maybe ChessPiece -> Maybe ChessPiece -> Position -> Distance -> (Bool, String)

-- Pawn validation
isValidMoveHelper (Just (ChessPiece Pawn White)) _ _ (0, 1) = (True, "Good Move!")
isValidMoveHelper (Just (ChessPiece Pawn White)) _ (Position _ 2) (0, 2) = (True, "Good Move!")

isValidMoveHelper (Just (ChessPiece Pawn Black)) _ _ (0, (-1)) = (True, "Good Move!")
isValidMoveHelper (Just (ChessPiece Pawn Black)) _ (Position _ 7) (0, (-2)) = (True, "Good Move!")

isValidMoveHelper (Just (ChessPiece pieceName _)) _ _ d = case pieceName of
    Rook -> (isLinear d, "")
    Knight -> (isEl d, "")
    Bishop -> (isDiagnal d, "")
    Queen -> (isLinearXorDiagnal d, "")
    King -> (isLinearXorDiagnal d, "") 
    _ ->  (False, "Invalid Move")
    

isValidMoveHelper _ _ _ _ = (False, "Invalid Move")

isPathClear :: Position -> Distance -> GameSetup -> Bool
isPathClear p1 d setup = and innerMappedToNothing 
    where innerPositions = (init . tail) ps
          lookedUpPositions = map (`Map.lookup` setup) innerPositions
          innerMappedToNothing = map (== Nothing) lookedUpPositions 
          ps = positions p1 d

executeMove :: Color -> Position -> Position -> GameSetup -> (GameSetup, Color, String)
executeMove turn p1 p2 setup  
    | valid = (Map.insert p2 (fromJust $ Map.lookup p1 setup) (Map.delete p1 setup), toggleColor turn, msg)
    | otherwise = (setup, turn, msg)
        where (valid, msg) = isValidMove turn p1 p2 setup 

