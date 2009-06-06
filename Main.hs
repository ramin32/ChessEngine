----------------------------------------------------------
-- Main.hs
--
-- Main module, starts game.
--
-- Author: 
-- Ramin Rakhamimov
-- http://raminrakhamimov.tk
-- ramin32@gmail.com
---------------------------------------------------------


import GameSetup
import Game
import ChessPiece

main = do 
    let setup = newGameSetup
    let turn = White
    runGame setup turn

