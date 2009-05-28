module GameSetup where

import qualified Data.Map as Map
import Data.List
import Data.Maybe

import ChessPiece
import Position
import StringUtil

type PartialSetup = Map.Map Position ChessPiece

data GameSetup = GameSetup { whitePieces :: PartialSetup,
                             blackPieces :: PartialSetup } deriving (Eq, Ord)   



pawnsSetup :: Color -> PartialSetup
pawnsSetup color = Map.fromList $ 
                         zip (positionsByRank rank) (repeat $ ChessPiece Pawn color) 
                         where 
                            rank = if color == White then 2 else 7

otherPiecesSetup :: Color -> PartialSetup
otherPiecesSetup color = Map.fromList $
                         [(p, ChessPiece n c) | (p, n, c) <- zip3 (positionsByRank rank) pieces (repeat color)]
                         where 
                            pieces = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook] 
                            rank = if color == White then 1 else 8
            
newGameSetup :: GameSetup
newGameSetup = GameSetup 
               (Map.union (pawnsSetup White) (otherPiecesSetup White))
               (Map.union (pawnsSetup Black) (otherPiecesSetup Black))

singleView :: GameSetup -> PartialSetup
singleView setup = Map.union (whitePieces setup) (blackPieces setup)

pieceAt :: Position -> GameSetup -> Maybe ChessPiece
pieceAt p setup = Map.lookup p (singleView setup)

piecesByRank :: GameSetup -> Int -> [Maybe ChessPiece]
piecesByRank setup r = map (\p -> pieceAt p setup) (positionsByRank r)

instance Show GameSetup where
    show setup = intercalate "\n" ((surround header (stringifySetup setup)) ++ fileLegend)
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




