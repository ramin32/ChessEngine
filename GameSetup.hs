module GameSetup where

import qualified Data.Map as Map
import Data.List

import ChessPiece

data Position = Position {file :: Char, rank :: Int} deriving (Eq, Ord, Show)

type GameSetup = Map.Map Position ChessPiece

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

stringifySetup setup = intercalate "\n" [fullIntercalate "|" $ map (\x -> pieceAt x setup) (positionsByRank r) | r <- [8, 7..1]]

printSetup :: GameSetup -> IO ()
printSetup setup = do
                       putStrLn $ surround "+" $ replicate 23 '-'
                       putStrLn $ stringifySetup setup 
                       putStrLn $ surround "+" $ replicate 23 '-'
