module Chess
( Position
, Piece
, Color
, ChessPiece
) where

-- Position File, Rank
type Position = (Char, Int)

data Piece = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Ord, Show, Read) 

data Color = White | Black deriving (Eq, Show, Read)

type ChessPiece = (Piece, Color, Position) 


 

