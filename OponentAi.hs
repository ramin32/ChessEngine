module OponentAi where

import qualified Data.Map as Map
import Data.List
import Data.Ord

import Position
import ChessPiece
import MoveValidator
import GameSetup

calculateMove :: Color -> GameSetup -> Move
calculateMove c setup = fst $ minimumBy (comparing snd) $ moveScores (allValidMoves c setup) setup

moveScores :: [Move] -> GameSetup ->  [(Move, Int)]
moveScores moves setup = map (\m -> (m, evaluateSetup $ unsafeExecuteMove m setup)) moves

allValidMoves :: Color -> GameSetup -> [Move]
allValidMoves c setup = [Move p1 p2 | p1 <- allPositions, p2 <- allPositions, fst $ isValidMove c (Move p1 p2) setup]
