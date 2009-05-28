module GameSetup where

import qualified Data.Map as Map
import Data.List
import Data.Maybe

import ChessPiece
import Position
import StringUtil

type GameSetup = Map.Map Position ChessPiece

pawnsSetup :: Color -> GameSetup
pawnsSetup color = Map.fromList $ 
                         zip (positionsByRank rank) (repeat $ ChessPiece Pawn color) 
                         where 
                            rank = if color == White then 2 else 7

otherPiecesSetup :: Color -> GameSetup
otherPiecesSetup color = Map.fromList $
                         [(p, ChessPiece n c) | (p, n, c) <- zip3 (positionsByRank rank) pieces (repeat color)]
                         where 
                            pieces = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook] 
                            rank = if color == White then 1 else 8
            
newGameSetup :: GameSetup
newGameSetup = Map.union 
               (Map.union (pawnsSetup White) (otherPiecesSetup White))
               (Map.union (pawnsSetup Black) (otherPiecesSetup Black))

piecesByRank :: GameSetup -> Int -> [Maybe ChessPiece]
piecesByRank setup r = map (\p -> Map.lookup p setup) (positionsByRank r)

showSetup :: GameSetup -> String
showSetup setup = intercalate "\n" ((surround header (stringifySetup setup)) ++ fileLegend)
        where
            stringifySetup :: GameSetup -> [String]
            stringifySetup setup = concat [prettyRank setup r | r <- [8, 7..1]]

            header :: [String]
            header = ["--" ++ (surround "+" $ replicate 23 '-')]

            fileLegend :: [String]
            fileLegend = ["  |A |B |C |D |E |F |G |H |" ]

            prettyRank :: GameSetup -> Int -> [String]
            prettyRank setup r = [(show r) ++ " " ++ (fullIntercalate "|" $ cleanPiecesByRank setup r)]

            cleanPiecesByRank :: GameSetup -> Int -> [String]
            cleanPiecesByRank setup r = map showMaybe $ piecesByRank setup r




