module PositionIterator where

import qualified Data.Map as Map

import Position
import ChessPiece
import MoveValidator
import GameSetup


calculateMove :: Color -> GameSetup -> (Position, Position)
calculateMove c setup = snd $ 
                        head $ 
                        filter (\((b, _), _) -> b) 
                        [(isValidMove c p1 p2 setup, (p1, p2)) | p1 <- allPositions, p2 <- allPositions ]
