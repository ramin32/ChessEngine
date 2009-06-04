module Game where

import Data.Char
import qualified Data.Map as Map
import Data.Maybe

import GameSetup
import Position
import MoveValidator
import ChessPiece
import PositionIterator


runGame :: GameSetup -> Color -> IO ()
runGame setup White = do
    putStrLn $ showSetup setup
    print "Enter your move (a2 a3)"
    move <- getLine
    let p1 = takeWhile (/= ' ') move
    let p2 = (tail . dropWhile (/= ' ')) move
    let pos1 = Position (p1 !! 0) (digitToInt $ p1 !! 1)
    let pos2 = Position (p2 !! 0) (digitToInt $ p2 !! 1)
    let (newSetup, newTurn, newMsg) =  executeMove White pos1 pos2 setup
    putStrLn newMsg
    runGame newSetup newTurn

runGame setup Black = do
    let (pos1, pos2) = calculateMove Black setup
    let (newSetup, newTurn, newMsg) =  executeMove Black pos1 pos2 setup
    runGame newSetup newTurn
    
executeMove :: Color -> Position -> Position -> GameSetup -> (GameSetup, Color, String)
executeMove turn p1 p2 setup  
    | valid = (Map.insert p2 (fromJust $ Map.lookup p1 setup) (Map.delete p1 setup), toggleColor turn, msg)
    | otherwise = (setup, turn, msg)
        where (valid, msg) = isValidMove turn p1 p2 setup 

