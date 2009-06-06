----------------------------------------------------------
-- Game.hs
--
-- Runs a game session alternating between user and CPU.
--
-- Author: 
-- Ramin Rakhamimov
-- http://raminrakhamimov.tk
-- ramin32@gmail.com
---------------------------------------------------------

module Game where

import Data.Char
import qualified Data.Map as Map
import Data.Maybe

import GameSetup
import Position
import MoveValidator
import ChessPiece
import OponentAi


runGame :: GameSetup -> Color -> IO ()
runGame setup White = do
    putStrLn $ showSetup setup
    putStrLn "Enter your move (a2 a3)"
    move <- getLine
    let p1 = takeWhile (/= ' ') move
    let p2 = (tail . dropWhile (/= ' ')) move
    let pos1 = Position (p1 !! 0) (digitToInt $ p1 !! 1)
    let pos2 = Position (p2 !! 0) (digitToInt $ p2 !! 1)
    let (newSetup, newTurn, newMsg) =  executeMove White (Move pos1 pos2) setup
    putStrLn newMsg
    runGame newSetup newTurn

runGame setup Black = do
    let (m, score) = calculateMove Black setup
    let scoreString = "Score: " ++ (show score)
    putStrLn scoreString
    let newSetup = unsafeExecuteMove m setup
    runGame newSetup White
    
executeMove :: Color -> Move -> GameSetup -> (GameSetup, Color, String)
executeMove turn m setup  
    | valid = (unsafeExecuteMove m setup, toggleColor turn, msg)
    | otherwise = (setup, turn, msg)
        where (valid, msg) = isValidMove turn m  setup 
