module OponentAi where

import qualified Data.Map as Map
import Data.List
import Data.Ord

import Position
import ChessPiece
import MoveValidator
import GameSetup

calculateMove :: Color -> GameSetup -> Move
calculateMove c setup = fst $ minimumBy (comparing snd) $ moveScores possibleCMoves setup
    where possibleCMoves = allValidMoves c setup
          possibleToggledCMoves = allValidMoves (toggleColor c) setup

moveScores :: [Move] -> GameSetup ->  [(Move, Int)]
moveScores moves setup = map (\m -> (m, moveScore m setup)) moves

moveScore :: Move -> GameSetup -> Int
moveScore move setup = mobilityScore newSetup + evaluateSetup newSetup
    where newSetup = unsafeExecuteMove move setup

allValidMoves :: Color -> GameSetup -> [Move]
allValidMoves c setup = [Move p1 p2 | p1 <- allPositions, p2 <- allPositions, fst $ isValidMove c (Move p1 p2) setup]


mobilityScore :: GameSetup -> Int
mobilityScore setup = (length $ allValidMoves White setup) - (length $ allValidMoves Black setup)
