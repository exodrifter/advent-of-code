module Main where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Safe

-- https://www.twitch.tv/videos/1991925197
main :: IO ()
main = do
  input <- readFile "data/2023-day01.txt"

  putStr "Part One: "
  print (sum <$> extractAllNumbers digitMapping input)
  putStr "Part Two: "
  print (sum <$> extractAllNumbers (digitMapping ++ writtenMapping) input)

-- Given a multi-line string, extract all of the two-digit numbers or return
-- Nothing.
extractAllNumbers :: [(Int, String)] -> String -> Maybe [Int]
extractAllNumbers mapping input =
  sequence (extractNumber mapping <$> lines input)

-- Given a single line, extract the two-digit number or return Nothing.
extractNumber :: [(Int, String)] -> String -> Maybe Int
extractNumber mapping input = do
  tens <- extractDigitPrefix mapping input
  -- The last digit is the same as the first digit of the string in reverse
  ones <- extractDigitPrefix (fmap reverse <$> mapping) (reverse input)
  pure (toTwoDigitNumber tens ones)

-- Given a single line, extract the first digit.
extractDigitPrefix :: [(Int, String)] -> String -> Maybe Int
extractDigitPrefix mapping input =
  let
    firstSuccess = Safe.headMay . Maybe.catMaybes
  in
    case input of
      [] -> Nothing
      _ ->
        case firstSuccess (flip extractDigit input <$> mapping) of
          Nothing -> extractDigitPrefix mapping (drop 1 input)
          Just answer -> Just answer

-- Attempt to extract the digit at the beginning of the string.
extractDigit :: (Int, String) -> String -> Maybe Int
extractDigit (int, key) input =
  if List.isPrefixOf key input
  then Just int
  else Nothing

--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------

-- A mapping of digits to their corresponding integer
digitMapping :: [(Int, String)]
digitMapping =
  [ (1, "1")
  , (2, "2")
  , (3, "3")
  , (4, "4")
  , (5, "5")
  , (6, "6")
  , (7, "7")
  , (8, "8")
  , (9, "9")
  , (0, "0")
  ]

-- A mapping of written out digits to their corresponding integer
writtenMapping :: [(Int, String)]
writtenMapping =
  [ (1, "one")
  , (2, "two")
  , (3, "three")
  , (4, "four")
  , (5, "five")
  , (6, "six")
  , (7, "seven")
  , (8, "eight")
  , (9, "nine")
  ]

-- Convert two integers into one two digit number.
toTwoDigitNumber :: Int -> Int -> Int
toTwoDigitNumber tens ones = tens * 10 + ones
