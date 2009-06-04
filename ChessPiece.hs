module ChessPiece where

data PieceName = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Ord, Show, Enum) 

data Color = White | Black deriving (Eq, Ord, Show, Enum)

data ChessPiece = ChessPiece {name :: PieceName, color :: Color} deriving (Eq, Ord)

toggleColor :: Color -> Color
toggleColor White = Black
toggleColor Black = White

instance Show ChessPiece where
    show p = ([head $ show $ color p]) ++ initial
        where 
        initial = [if (name p) == Knight then 'N' else (head $ show $ name p)]

