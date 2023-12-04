module Year2023.Day02
( main
) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Safe

-- https://vods.exodrifter.space/2023/12/02/0732
main :: IO ()
main = do
  input <- T.pack <$> readFile "data/2023-day02.txt"
  putStr "Part One: "
  print (sum <$> traverse (extractValidGame maximums) (T.lines input))
  putStr "Part Two: "
  print (sum <$> traverse extractGamePower (T.lines input))

--------------------------------------------------------------------------------
-- Part One
--------------------------------------------------------------------------------

maximums :: Map.Map CubeColor Int
maximums =
  Map.fromList
    [ (Red, 12)
    , (Green, 13)
    , (Blue, 14)
    ]

-- Returns the game number for valid games and 0 otherwise.
extractValidGame :: Map.Map CubeColor Int -> T.Text -> Maybe Int
extractValidGame maximums input = do
  let exceedsMaximums hand =
           Map.lookup Red maximums < Map.lookup Red hand
        || Map.lookup Green maximums < Map.lookup Green hand
        || Map.lookup Blue maximums < Map.lookup Blue hand
  (gameNumber, hands) <- extractGameInfo input
  if any exceedsMaximums hands
  then pure 0
  else pure gameNumber

--------------------------------------------------------------------------------
-- Part Two
--------------------------------------------------------------------------------

-- Returns the game power for a game.
extractGamePower :: T.Text -> Maybe Int
extractGamePower input = do
  let maximums :: [Hand] -> Hand
      maximums hands =
        let -- Not all hands have every single cube color
          reds = Maybe.fromMaybe 0 . Map.lookup Red <$> hands
          greens = Maybe.fromMaybe 0 . Map.lookup Green <$> hands
          blues = Maybe.fromMaybe 0 . Map.lookup Blue <$> hands
        in
          Map.fromList
            [ (Red, maximum reds)
            , (Green, maximum greens)
            , (Blue, maximum blues)
            ]

      calculateSetPower :: Hand -> Maybe Int
      calculateSetPower set = do
        red <- Map.lookup Red set
        green <- Map.lookup Green set
        blue <- Map.lookup Blue set
        pure (red * green * blue)

  (gameNumber, hands) <- extractGameInfo input
  calculateSetPower (maximums hands)

--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------

type Hand = Map.Map CubeColor Int

data CubeColor = Red | Green | Blue
  deriving stock (Eq, Ord, Show)

-- Given a line of text, extract the information about the game.
extractGameInfo :: T.Text -> Maybe (Int, [Hand])
extractGameInfo input = do
  let (gameString, handStrings) = T.drop 2 <$> T.break (== ':') input
  gameNumber <- readMay (T.drop 5 gameString)
  hands <- traverse extractHand (T.splitOn "; " handStrings)
  pure (gameNumber, hands)

-- Given a single set of cubes in a game, extract the hand information
extractHand :: T.Text -> Maybe Hand
extractHand input =
  let cubes = T.splitOn ", " input
  in  Map.fromList <$> traverse extractCubes cubes

-- Given a single count of cubes in a game, extract the number of cubes
extractCubes :: T.Text -> Maybe (CubeColor, Int)
extractCubes input = do
  let (countText, color) = T.drop 1 <$> T.break (== ' ') input
  count <- readMay countText
  case color of
    "red" -> pure (Red, count)
    "green" -> pure (Green, count)
    "blue" -> pure (Blue, count)
    _ -> Nothing

readMay :: (Read a) => T.Text -> Maybe a
readMay = Safe.readMay . T.unpack
