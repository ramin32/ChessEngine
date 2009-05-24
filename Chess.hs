module Chess where

-- Position File, Rank
type Position = (Char, Int)

data PieceName = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Ord, Show, Enum) 

data Color = White | Black deriving (Eq, Show)

type ChessPiece = (PieceName, Color, Position) 

positionsByRank :: Int -> [Position]
positionsByRank rank = zip ['a'..'h'] (cycle [rank]) 

pawns :: Color -> Int -> [ChessPiece]
pawns color rank  = [(Pawn, color, pos) | pos <- positionsByRank rank]

whitePawns = pawns White 2
blackPawns = pawns Black 7

otherPieces :: Color -> Int -> [ChessPiece]
otherPieces color rank = zip3 [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook] 
                              (cycle [color]) 
                              (positionsByRank rank)

whiteOtherPieces = otherPieces White 1
blackOtherPieces = otherPieces Black 8

newGame :: [ChessPiece]
newGame = concat [whitePawns, whiteOtherPieces, blackPawns, blackOtherPieces]
