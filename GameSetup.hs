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

pawns :: Color -> Int -> GameSetup
pawns color rank  = createGameSetup 
                    [(position, ChessPiece Pawn color) | position <- positionsByRank rank]

whitePawnRank = 2
blackPawnRank = 7
whitePawns = pawns White whitePawnRank
blackPawns = pawns Black blackPawnRank

otherPieces :: Color -> Int -> GameSetup
otherPieces color rank = createGameSetup [(p ,ChessPiece n c)
                         | (n, c, p) <- zip3 [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook] 
                                             (cycle [color]) 
                                             (positionsByRank rank)]

whiteOthersRank = 1
blackOthersRank = 8
whiteOtherPieces = otherPieces White whiteOthersRank
blackOtherPieces = otherPieces Black blackOthersRank

newGameSetup :: GameSetup
newGameSetup = Map.union 
               (Map.union whitePawns whiteOtherPieces)
               (Map.union blackPawns blackOtherPieces)

