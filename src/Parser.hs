module Parser
( Parser
, parse
, parseMany

, lexeme

-- Re-exports
, Lexer.decimal
, Combinators.many
) where

import Data.Void (Void)

import qualified Control.Monad.Combinators as Combinators
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Text.Megaparsec.Error as Error

type Parser = Mega.Parsec Void T.Text

parse :: Parser a -> FilePath -> IO a
parse parser file = do
  input <- TIO.readFile file
  case Mega.runParser parser file input of
    Left err -> error (Error.errorBundlePretty err)
    Right result -> pure result

parseMany :: Parser a -> FilePath -> IO [a]
parseMany parser = parse (Combinators.many parser)

lexeme :: Parser a -> Parser a
lexeme =
  Lexer.lexeme Char.space
