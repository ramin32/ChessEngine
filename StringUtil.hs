module StringUtil where

import Data.List

import ChessPiece

surround :: [a] -> [a] -> [a]
surround s1 s2 = s1 ++ s2 ++ s1

fullIntercalate :: String -> [String] -> String
fullIntercalate str list = surround str $ intercalate str list

showMaybe :: Maybe ChessPiece -> String
showMaybe Nothing = "  "
showMaybe (Just a) = show a

