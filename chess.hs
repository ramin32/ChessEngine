module Chess where

data Move = Left | Right | Up | Down
type Position = (Char, Int)

executeMove :: Position -> Move -> Position
executeMove p m 
            | m == Chess.Left = ((fst p) + 1, snd p)
            | m == Chess.Right = ((fst p) - 1, snd p)
            | m == Chess.Up = (fst p, (snd p) + 1)
            | m == Chess.Down = (fst p, (snd p) - 1)
