module Game where

import Data.Char

import GameSetup
import Position
import MoveValidator
import ChessPiece


runGame :: GameSetup -> Color -> IO ()
runGame setup turn = do
    putStrLn $ showSetup setup
    print "Enter your move (a2 a3)"
    move <- getLine
    let p1 = takeWhile (/= ' ') move
    let p2 = (tail . dropWhile (/= ' ')) move
    let pos1 = Position (p1 !! 0) (digitToInt $ p1 !! 1)
    let pos2 = Position (p2 !! 0) (digitToInt $ p2 !! 1)
    runGame (executeMove turn pos1 pos2 setup) (toggleColor turn)
    

