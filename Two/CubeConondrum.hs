{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Two.CubeConondrum where

import Control.Monad (void)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

-- Common

type Parser = Parsec Void Text

data Color where
  Red :: Color
  Green :: Color
  Blue :: Color
  deriving (Eq)

parseColor :: Parser (Color, Int)
parseColor = do
  void $ optional (char ' ')
  numberOfCubes <- read @Int <$> some digitChar
  void (char ' ')
  color <-
    choice
      [ Red <$ parseColor' "red",
        Green <$ parseColor' "green",
        Blue <$ parseColor' "blue"
      ]
  return (color, numberOfCubes)
  where
    parseColor' :: Text -> Parser ()
    parseColor' c = try $ void $ string c <* optional (char ',')

parseRevelation :: Parser (Int, Int, Int)
parseRevelation = do
  colors <- some $ parseColor <* optional (char ' ')
  let findColor c = fromMaybe 0 (lookup c colors)
  pure (findColor Red, findColor Green, findColor Blue)

data Game where
  Game :: {gameId :: Int, revelations :: [(Int, Int, Int)]} -> Game

parseGame :: Parser Game
parseGame = do
  void (string "Game")
  void (char ' ')
  gameId <- read @Int <$> some digitChar
  void (char ':')
  revelations <- some (parseRevelation <* optional (char ';')) <* eof
  return Game {..}

rawInput :: IO [Text]
rawInput = T.lines <$> TIO.readFile "Two/input.txt"

input :: IO [Game]
input = map getGame <$> rawInput
  where
    getGame :: Text -> Game
    getGame s = case parse parseGame "" s of
      Left _ -> error ":("
      Right game -> game

-- Part 1

isPossible :: (Int, Int, Int) -> [(Int, Int, Int)] -> Bool
isPossible _ [] = True
isPossible maxPerColor@(rMax, gMax, bMax) ((r, g, b) : xs)
  | (r <= rMax) && (g <= gMax) && (b <= bMax) = isPossible maxPerColor xs 
  | otherwise = False

partOne :: [Game] -> Int
partOne inp =
  sum . map gameId $
    filter (isPossible (12, 13, 14) . revelations) inp

-- Part 2

power :: [(Int, Int, Int)] -> Int
power revelations = go revelations (0,0,0)
  where
    go [] (rMin, gMin, bMin) = rMin * gMin * bMin
    go ((r, g, b) : xs) (rMin, gMin, bMin) = go xs (max r rMin, max g gMin, max b bMin)

partTwo :: [Game] -> Int
partTwo inp = sum $ map (power . revelations) inp

---

main :: IO ()
main = do
  inp <- input
  print $ partOne inp
  print $ partTwo inp
