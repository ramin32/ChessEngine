module Chess where

-- Position File, Rank
type Position = (Char, Int)

data PieceName = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Ord, Show, Enum) 

data Color = White | Black deriving (Eq, Show)

data ChessPiece = ChessPiece { name :: PieceName
                             , color :: Color
                             , position :: Position}
                  deriving (Eq, Show)

type GameSetup = [ChessPiece]

positionsByRank :: Int -> [Position]
positionsByRank rank = zip ['a'..'h'] (cycle [rank]) 

pawns :: Color -> Int -> GameSetup
pawns color rank  = [ChessPiece {name = Pawn, color = color, position = pos} | pos <- positionsByRank rank]

whitePawns = pawns White 2
blackPawns = pawns Black 7

otherPieces :: Color -> Int -> GameSetup
otherPieces color rank = [ChessPiece {name = n, color = c, position = p} 
                         | (n, c, p) <- zip3 [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook] 
                                            (cycle [color]) 
                                            (positionsByRank rank)]

whiteOtherPieces = otherPieces White 1
blackOtherPieces = otherPieces Black 8

newGameSetup :: GameSetup
newGameSetup = concat [whitePawns, whiteOtherPieces, blackPawns, blackOtherPieces]

pieceAt :: GameSetup -> Position -> Maybe ChessPiece
pieceAt [] pos = Nothing
pieceAt (x:xs) pos
    | (position x) == pos = Just x
    | otherwise = pieceAt xs pos
