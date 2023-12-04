module Year2023.Day04
( main
) where

import Control.Applicative ((<|>))
import Control.Monad.Combinators (many)
import Data.Void (Void)

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Safe
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Text.Megaparsec.Error as Error
import qualified Text.Megaparsec.Pos as Pos
import qualified Data.Text.IO as TIO

import Debug.Trace
main :: IO ()
main = do
  cards <- parseInput "data/2023-day04.txt"

  putStr "Part One: "
  print (sum (score <$> cards))
  putStr "Part Two: "
  print (totalScratchcards cards)

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
    initialMap :: Map.Map Int Int
    initialMap =
      Map.fromList ((\card -> (cardNumber card, 1)) <$> cards)

    go :: Map.Map Int Int -> Card -> Map.Map Int Int
    go acc card =
      let
        copies = Maybe.fromMaybe 0 (Map.lookup (cardNumber card) acc)
        adjustCards acc index = Map.adjust (+ copies) index acc
        firstCard = cardNumber card + 1
        lastCard = cardNumber card + numberOfMatches card
      in
        List.foldl' adjustCards acc [firstCard..lastCard]

    counts :: [(Int, Int)]
    counts = Map.toList (List.foldl' go initialMap cards)

  in
    sum (snd <$> counts)

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

data Card = Card { cardNumber :: Int, winningNumbers :: [Int], myNumbers :: [Int] }
  deriving Show

type Parser = Mega.Parsec Void T.Text

parseInput :: FilePath -> IO [Card]
parseInput file = do
  input <- TIO.readFile file
  case Mega.runParser (many card) file input of
    Left err -> error (Error.errorBundlePretty err)
    Right result -> pure result

card :: Parser Card
card = do
  _ <- lexeme "Card"
  number <- Lexer.decimal
  _ <- lexeme ":"
  winning <- many (lexeme Lexer.decimal)
  _ <- lexeme "|"
  mine <- many (lexeme Lexer.decimal)
  pure (Card number winning mine)

lexeme :: Parser a -> Parser a
lexeme =
  Lexer.lexeme Char.space
