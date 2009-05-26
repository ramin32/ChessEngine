module GameSetup where

import qualified Data.Map as Map
import Data.List
import Data.Char

import ChessPiece

data Position = Position {file :: Char, rank :: Int} deriving (Eq, Ord, Show)
type GameSetup = Map.Map Position ChessPiece

fileOrd :: Position -> Int
fileOrd p = ord $ file p


difference :: Position -> Position -> (Int, Int)
difference p1 p2 = (abs $ (fileOrd p1) - (fileOrd p2), abs $ (rank p1) - (rank p2))

createGameSetup :: [(Position, ChessPiece)] -> GameSetup
createGameSetup list = Map.fromList list 

positionsByRank :: Int -> [Position]
positionsByRank rank = [Position f r | (f, r) <- zip ['a'..'h'] (repeat rank) ]

pawnsSetup :: Color -> GameSetup
pawnsSetup color = createGameSetup $ 
                         zip (positionsByRank rank) (repeat $ ChessPiece Pawn color) 
                         where 
                            rank = if color == White then 2 else 7

otherPiecesSetup :: Color -> GameSetup
otherPiecesSetup color = createGameSetup $
                         [(p, ChessPiece n c) | (p, n, c) <- zip3 (positionsByRank rank) pieces (repeat color)]
                         where 
                            pieces = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook] 
                            rank = if color == White then 1 else 8
            
newGameSetup :: GameSetup
newGameSetup = Map.union 
               (Map.union (pawnsSetup White) (otherPiecesSetup White))
               (Map.union (pawnsSetup Black) (otherPiecesSetup Black))

cleanMaybe :: (Show a) => Maybe a -> String
cleanMaybe Nothing = "  "
cleanMaybe (Just a) = show a

pieceAt x setup = cleanMaybe $ Map.lookup x setup

surround :: String -> String -> String
surround s1 s2 = s1 ++ s2 ++ s1
fullIntercalate str list = surround str $ intercalate str list

stringifySetup setup = intercalate "\n" 
                       [(show r) ++ " " ++ (fullIntercalate "|" $ map (\x -> pieceAt x setup) (positionsByRank r) )
                       | r <- [8, 7..1]]

printSetup :: GameSetup -> IO ()
printSetup setup = do
    putStrLn $ "--" ++ (surround "+" $ replicate 23 '-')
    putStrLn $ stringifySetup setup 
    putStrLn $ "--" ++ (surround "+" $ replicate 23 '-')
    putStrLn "  |A |B |C |D |E |F |G |H |" 

onBoard :: Position -> Bool
onBoard p 
    | file p < 'a' || file p > 'h' = False
    | rank p < 1 || rank p > 8 = False
    | otherwise = True


clearPath :: Position -> Position -> GameSetup -> Bool
clearPath p1 p2 setup
    | p1 == p2 = False
    | not (onBoard p1 || onBoard p2) = False
    | otherwise = Nothing == Map.lookup p2 setup
    where diff = difference p1 p2 

