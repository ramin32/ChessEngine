module Chess where

data Move = Left | Right | Up | Down deriving (Eq)
type Position = (Char, Int)

executeMove position move 
    | move == Chess.Left = ((fst position) + 1, snd position)
    | move == Chess.Right = ((fst position) - 1, snd position)
    | move == Chess.Up = (fst position, (snd position) + 1)
    | move == Chess.Down = (fst position, (snd position) - 1)
