----------------------------------------------------------
-- MoveValidator.hs
--
-- Contains various functions to validate valid moves
-- given certain GameSetup's.
-- 
-- Author: 
-- Ramin Rakhamimov
-- http://raminrakhamimov.tk
-- ramin32@gmail.com
---------------------------------------------------------


module MoveValidator where

import qualified Data.Map as Map
import Data.Maybe
import Control.Applicative

import Position
import ChessPiece
import GameSetup


isValidMove :: Color -> Move -> GameSetup -> (Bool, String)
isValidMove turn m setup 
    | isCheckMated turn setup = (False, "CheckMate!")
    | isChecked turn setup = case (elem m $ allCheckSaves turn setup) of
                             True -> (True, "Good Move")
                             False -> (False, "Still checked!")
    | cp1DoesntExists = (False, "No piece at initial location!")
    | samePosition = (False, "Same position!")
    | (fmap color cp1) /= Just turn = (False, "Wrong turn!") 
    | not (onBoard p1 || onBoard p2) = (False, "Move out of bounds!")
    | oponentMatchingColors = (False, "You're the opponent!")  
    | cp1IsNotKnight && not (isPathClear p1 d setup) = (False, "Path not clear")
    | otherwise = isValidMoveHelper cp1 cp2 p1 d
        where p1 = from m
              p2 = to m
              cp1 = Map.lookup p1 setup
              cp2 = Map.lookup p2 setup
              d = distance p1 p2
              samePosition = p1 == p2
              oponentMatchingColors = (fmap color cp1) == (fmap color cp2)
              cp1DoesntExists = cp1 == Nothing
              cp1IsNotKnight = (fmap name cp1) /= Just Knight
              

isValidMoveHelper :: Maybe ChessPiece -> Maybe ChessPiece -> Position -> Distance -> (Bool, String)

-- Don't take king.
isValidMoveHelper _ (Just (ChessPiece King _)) _ _ = (False, "King cannot be taken!")

-- Pawn validation
isValidMoveHelper (Just (ChessPiece Pawn White)) _ _ (0, 1) = (True, "Good Move!")
isValidMoveHelper (Just (ChessPiece Pawn White)) _ (Position _ 2) (0, 2) = (True, "Good Move!")
isValidMoveHelper (Just (ChessPiece Pawn  White)) (Just (ChessPiece _ Black)) _ (f, 1) = (abs f == 1, "Good Move!")

isValidMoveHelper (Just (ChessPiece Pawn Black)) _ _ (0, (-1)) = (True, "Good Move!")
isValidMoveHelper (Just (ChessPiece Pawn Black)) _ (Position _ 7) (0, (-2)) = (True, "Good Move!")
isValidMoveHelper (Just (ChessPiece Pawn Black)) (Just (ChessPiece _ White)) _ (f, (-1)) = (abs f == 1, "Good Move!")

isValidMoveHelper (Just (ChessPiece pieceName _)) _ _ d = case pieceName of
    Rook -> (isLinear d, "")
    Knight -> (isEl d, "")
    Bishop -> (isDiagnal d, "")
    Queen -> (isLinearXorDiagnal d, "")
    King -> (isLinearXorDiagnal d && isSingleMover d, "") 
    _ ->  (False, "Invalid Move")
    

isValidMoveHelper _ _ _ _ = (False, "Invalid Move")

isPathClear :: Position -> Distance -> GameSetup -> Bool
isPathClear p1 d setup = and innerMappedToNothing 
    where innerPositions = (init . tail) ps
          lookedUpPositions = map (`Map.lookup` setup) innerPositions
          innerMappedToNothing = map (== Nothing) lookedUpPositions 
          ps = positions p1 d

isChecked :: Color -> GameSetup -> Bool
isChecked c setup = check
    where kingPosition = fromJust $ piecePosition (ChessPiece King c) setup -- Kings always on board!
          allOponentMoves = allValidMoves (toggleColor c) setup
          check = not $ null (filter (\m -> (to m) == kingPosition) allOponentMoves)
          
isCheckMated :: Color -> GameSetup -> Bool
isCheckMated c setup = null $ allCheckSaves c setup

allCheckSaves :: Color -> GameSetup -> [Move]
allCheckSaves c setup = filter (\m -> isChecked c (unsafeExecuteMove m setup)) $ allValidMoves c setup

allValidMoves :: Color -> GameSetup -> [Move]
allValidMoves c setup = [Move p1 p2 | p1 <- allOccupiedPositions setup, p2 <- allPositions, fst $ isValidMove c (Move p1 p2) setup]
