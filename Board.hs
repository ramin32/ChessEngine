module GameSetup where

import qualified Data.Map as Map

-- Position File, Rank
type Position = (Char, Int)

type GameSetup = [ChessPiece]

positionsByRank :: Int -> [Position]
positionsByRank rank = zip ['a'..'h'] (cycle [rank]) 

pawns :: Color -> Int -> GameSetup
pawns color rank  = [(position, ChessPiece Pawn color) | position <- positionsByRank rank]

whitePawnRank = 2
blackPawnRank = 7
whitePawns = pawns White whitePawnRank
blackPawns = pawns Black blackPawnRank

otherPieces :: Color -> Int -> GameSetup
otherPieces color rank = [ChessPiece n c p 
                         | (n, c, p) <- zip3 [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook] 
                                             (cycle [color]) 
                                             (positionsByRank rank)]

whiteOthersRank = 1
blackOthersRank = 8
whiteOtherPieces = otherPieces White whiteOthersRank
blackOtherPieces = otherPieces Black blackOthersRank

newGameSetup :: GameSetup
newGameSetup = concat [whitePawns, whiteOtherPieces, blackPawns, blackOtherPieces]

pieceAt :: GameSetup -> Position -> Maybe ChessPiece
pieceAt [] pos = Nothing
pieceAt (x:xs) pos
    | (position x) == pos = Just x
    | otherwise = pieceAt xs pos
