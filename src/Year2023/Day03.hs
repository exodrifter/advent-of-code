module Year2023.Day03
( main
) where

import Control.Applicative ((<|>))
import Control.Monad.Combinators (many)
import Data.Void (Void)

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Safe
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Text.Megaparsec.Error as Error
import qualified Text.Megaparsec.Pos as Pos

main :: IO ()
main = do
  input <- T.pack <$> readFile "data/2023-day03.txt"
  putStr "Part One: "
  print (sum . findPartNumbers <$> parseInput input)
  putStr "Part Two: "
  print (sum . findGearRatios <$> parseInput input)

--------------------------------------------------------------------------------
-- Part One
--------------------------------------------------------------------------------

findPartNumbers :: [SchematicItem] -> [Int]
findPartNumbers items = do
  let extractValues item =
        case item of
          Number { value = value } ->
            (Just (item, value), Nothing)
          Symbol {} ->
            (Nothing, Just item)

  let (numbers, symbols) = unzip (extractValues <$> items)
  (number, value) <- Maybe.catMaybes numbers

  if any (isAdjacent number) (Maybe.catMaybes symbols)
  then pure value
  else mempty

isAdjacent :: SchematicItem -> SchematicItem -> Bool
isAdjacent symbol number =
  let
    inAdjacentRow =
         (getRow number - 1) <= getRow symbol
      && getRow symbol <= (getRow number + 1)
    inAdjacentCol =
          (    (getCol number - 1) <= getCol symbol
            && getCol symbol <= (getCol number + getSize number)
          )
      ||  (    (getCol symbol - 1) <= getCol number
            && getCol number <= (getCol symbol + getSize symbol)
          )
  in
    inAdjacentRow && inAdjacentCol

--------------------------------------------------------------------------------
-- Part One
--------------------------------------------------------------------------------

findGearRatios :: [SchematicItem] -> [Int]
findGearRatios items = do
  let (numbers, symbols) = List.partition isNumber items
  symbol <- symbols

  pure (getGearRatio symbol numbers)

getGearRatio :: SchematicItem -> [SchematicItem] -> Int
getGearRatio symbol numbers =
  case symbol of
    Symbol { name = name } | name == '*' ->
      case filter (isAdjacent symbol) numbers of
        [Number { value = a }, Number { value = b }] -> a * b
        _ -> 0
    _ ->
      0

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data SchematicItem =
    Number { value :: Int, row :: Int, col :: Int, size :: Int }
  | Symbol { name :: Char, row :: Int, col :: Int}
  deriving stock (Show)

getRow :: SchematicItem -> Int
getRow si =
  case si of
    Number { row = row } -> row
    Symbol { row = row } -> row

getCol :: SchematicItem -> Int
getCol si =
  case si of
    Number { col = col } -> col
    Symbol { col = col } -> col

getSize :: SchematicItem -> Int
getSize si =
  case si of
    Number { size = size } -> size
    Symbol {} -> 1

isNumber :: SchematicItem -> Bool
isNumber si =
  case si of
    Number {} -> True
    Symbol {} -> False

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

type Parser = Mega.Parsec Void T.Text

parseInput :: T.Text -> Either String [SchematicItem]
parseInput input =
  case Mega.runParser schematicItems "" input of
    Left err -> error (Error.errorBundlePretty err)
    Right result -> pure result

schematicItems :: Parser [SchematicItem]
schematicItems = do
  many schematicItem

schematicItem :: Parser SchematicItem
schematicItem = do
  skipBlanks
  Mega.try schematicNumber <|> schematicSymbol

schematicNumber :: Parser SchematicItem
schematicNumber = do
  pos <- Mega.getSourcePos
  start <- Mega.getOffset
  number <- Lexer.decimal
  end <- Mega.getOffset
  skipBlanks

  let
    row = Pos.unPos (Pos.sourceLine pos)
    col = Pos.unPos (Pos.sourceColumn pos)
  pure (Number number row col (end - start))

schematicSymbol :: Parser SchematicItem
schematicSymbol = do
  pos <- Mega.getSourcePos
  name <- Mega.satisfy (`notElem` ['.', '\n'])
  skipBlanks

  let
    row = Pos.unPos (Pos.sourceLine pos)
    col = Pos.unPos (Pos.sourceColumn pos)
  pure (Symbol name row col)

skipBlanks :: Parser ()
skipBlanks = do
  _ <- many (Char.char '.' <|> Char.char '\n')
  pure ()
