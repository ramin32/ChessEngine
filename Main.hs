import Control.Monad
import Data.Char

import GameSetup
import Position
import MoveValidator


main = do 
    let setup = newGameSetup
    runGame setup

runGame :: GameSetup -> IO ()
runGame setup = do
    putStrLn $ showSetup setup
    print "Enter your move (a2 a3)"
    move <- getLine
    let p1 = takeWhile (/= ' ') move
    let p2 = (tail . dropWhile (/= ' ')) move
    let pos1 = Position (p1 !! 0) (digitToInt $ p1 !! 1)
    let pos2 = Position (p2 !! 0) (digitToInt $ p2 !! 1)
    runGame $ executeMove pos1 pos2 setup

