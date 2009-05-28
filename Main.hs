import Control.Monad

import GameSetup
import Position

main = forever $ do 
    let setup = newGameSetup
    putStrLn $ showSetup setup
    print "Enter your move (file, rank) -> (new file, new rank)"
    move <- getLine
    print move       
