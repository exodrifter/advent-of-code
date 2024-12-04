module Year2024.Day04
( main
) where

import Data.Vector (Vector, (!?))
import qualified Data.Maybe as Maybe
import qualified Data.Vector as Vector
import Debug.Trace

type WordSearch = Vector (Vector Char)
data Tuple = Tuple { col :: Int, row :: Int }

main :: IO ()
main = do
  wordSearch <- readFile "data/2024-day04.txt"
  let
    grid = Vector.fromList (Vector.fromList <$> lines wordSearch)
    cols = Vector.length grid
    rows = maybe 0 Vector.length (grid !? 0)
    positions = Tuple <$> [0..cols] <*> [0..rows]

  putStr "Part One: "
  putStrLn (show (countXMAS grid positions))
  putStr "Part Two: "
  putStrLn (show (countX_MAS grid positions))

--------------------------------------------------------------------------------
-- Part One
--------------------------------------------------------------------------------

countXMAS :: WordSearch -> [Tuple] -> Int
countXMAS grid positions =
  let
    directions = Tuple <$> [-1, 0, 1] <*> [-1, 0, 1]
    strings = buildXMAS grid 4 <$> positions <*> directions
  in
    length (filter (== "XMAS") strings)

buildXMAS :: WordSearch -> Int -> Tuple -> Tuple -> String
buildXMAS grid 0 _ _ = ""
buildXMAS grid count pos delta =
  case getCh grid pos of
    Just ch -> ch:buildXMAS grid (count - 1) (addPos pos delta) delta
    Nothing -> ""

--------------------------------------------------------------------------------
-- Part Two
--------------------------------------------------------------------------------

countX_MAS :: WordSearch -> [Tuple] -> Int
countX_MAS grid positions =
  let
    checks = buildX_MAS grid <$> positions
    matches = (==) <$> checks <*> ["ASSMM", "AMMSS", "AMSMS", "ASMSM"]
  in
    length (filter (==True) matches)

buildX_MAS :: WordSearch -> Tuple -> String
buildX_MAS grid position =
  let
    positions = position
              : [ addPos position (Tuple c r) | c <- [-1, 1], r <- [-1, 1] ]
  in
    Maybe.mapMaybe (getCh grid) positions

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

addPos :: Tuple -> Tuple -> Tuple
addPos pos delta = Tuple (col pos + col delta) (row pos + row delta)

getCh :: WordSearch -> Tuple -> Maybe Char
getCh grid pos = (grid !? row pos) >>= (!? col pos)
