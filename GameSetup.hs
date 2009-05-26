module GameSetup where

import qualified Data.Map as Map
import Data.List

import ChessPiece

-- Position File, Rank
type Position = (Char, Int)

type GameSetup = Map.Map Position ChessPiece

createGameSetup :: [(Position, ChessPiece)] -> GameSetup
createGameSetup list = Map.fromList list 

positionsByRank :: Int -> [Position]
positionsByRank rank = zip ['a'..'h'] (cycle [rank]) 

pawnsSetup :: Color -> GameSetup
pawnsSetup color = createGameSetup $ 
                         zip (positionsByRank rank) (cycle [ChessPiece Pawn color]) 
                         where 
                            rank = if color == White then 2 else 7

otherPiecesSetup :: Color -> GameSetup
otherPiecesSetup color = createGameSetup $
                         [(p, ChessPiece n c) | (p, n, c) <- zip3 (positionsByRank rank) pieces (cycle [color])]
                         where 
                            pieces = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook] 
                            rank = if color == White then 1 else 8
            
newGameSetup :: GameSetup
newGameSetup = Map.union 
               (Map.union (pawnsSetup White) (otherPiecesSetup White))
               (Map.union (pawnsSetup Black) (otherPiecesSetup Black))

