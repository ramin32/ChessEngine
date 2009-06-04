import GameSetup
import Game
import ChessPiece

main = do 
    let setup = newGameSetup
    let turn = White
    runGame setup turn

