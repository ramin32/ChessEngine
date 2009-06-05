module ChessPiece where

data PieceName = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Ord, Show, Enum) 

data Color = White | Black deriving (Eq, Ord, Show, Enum)

data ChessPiece = ChessPiece {name :: PieceName, color :: Color} deriving (Eq, Ord)

value :: ChessPiece -> Int
value (ChessPiece name _) = case name of
     Pawn -> 1
     Knight -> 3
     Bishop -> 3
     Rook -> 5
     Queen -> 9
     King -> 200

toggleColor :: Color -> Color
toggleColor White = Black
toggleColor Black = White

instance Show ChessPiece where
    show p = ([head $ show $ color p]) ++ initial
        where 
        initial = [if (name p) == Knight then 'N' else (head $ show $ name p)]

