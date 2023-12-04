module Year2023.Day04
( main
) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Parser as P
import qualified Text.Megaparsec as Mega

import Debug.Trace
main :: IO ()
main = do
  cards <- P.parseMany card "data/2023-day04.txt"

  putStr "Part One: "
  print (sum (score <$> cards))
  putStr "Part Two: "
  print (totalScratchcards cards)

data Card =
  Card
    { cardNumber :: Int
    , winningNumbers :: [Int]
    , myNumbers :: [Int]
    }
  deriving Show

card :: P.Parser Card
card = do
  _ <- P.lexeme "Card"
  number <- P.decimal
  _ <- P.lexeme ":"
  winning <- P.many (P.lexeme P.decimal)
  _ <- P.lexeme "|"
  mine <- P.many (P.lexeme P.decimal)
  pure (Card number winning mine)

--------------------------------------------------------------------------------
-- Part One
--------------------------------------------------------------------------------

score :: Card -> Int
score card =
  case numberOfMatches card of
    0 -> 0
    n -> 2 ^ (n - 1)

numberOfMatches :: Card -> Int
numberOfMatches card =
  length (winningNumbers card `List.intersect` myNumbers card)

--------------------------------------------------------------------------------
-- Part Two
--------------------------------------------------------------------------------

totalScratchcards :: [Card] -> Int
totalScratchcards cards =
  let
    initialMap =
      Map.fromList ((\card -> (cardNumber card, 1)) <$> cards)

    go acc card =
      let
        copies = Map.findWithDefault 0 (cardNumber card) acc
        adjustCards acc index = Map.adjust (+ copies) index acc
        firstCard = cardNumber card + 1
        lastCard = cardNumber card + numberOfMatches card
      in
        List.foldl' adjustCards acc [firstCard..lastCard]

  in
    sum (Map.elems (List.foldl' go initialMap cards))
