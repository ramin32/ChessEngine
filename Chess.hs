module Chess where

data PieceName = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Ord, Show, Enum) 

data Color = White | Black deriving (Eq, Show)

data ChessPiece = ChessPiece {name :: PieceName, color :: Color} deriving (Eq, Show)

