module Trebuchet where

import Data.Maybe (mapMaybe)
import Data.Char (digitToInt, isDigit)
import Data.List (tails, isPrefixOf)

-- Common

input :: IO [String]
input = lines <$> readFile "One/input.txt"

-- Part 1

inputDigits :: IO [[Int]]
inputDigits = do
  inp <- input
  let filteredInput = map (filter isDigit) inp
      parsedInput = map (map (\c -> read @Int [c])) filteredInput
  return parsedInput

value :: [Int] -> Int
value [] = 0
value [n] = 10 * n + n
value ns = (head ns * 10) + last ns

partOne :: [[Int]] -> Int
partOne input = sum (map value input)

-- Part 2

inputNumbers :: IO [[Int]]
inputNumbers = map (mapMaybe maybeDigit . tails) <$> input

maybeDigit :: String -> Maybe Int
maybeDigit [] = Nothing
maybeDigit input@(x : _)
  | "one" `isPrefixOf` input = Just 1
  | "two" `isPrefixOf` input = Just 2
  | "three" `isPrefixOf` input = Just 3
  | "four" `isPrefixOf` input = Just 4
  | "five" `isPrefixOf` input = Just 5
  | "six" `isPrefixOf` input = Just 6
  | "seven" `isPrefixOf` input = Just 7
  | "eight" `isPrefixOf` input = Just 8
  | "nine" `isPrefixOf` input = Just 9
  | isDigit x = Just $ digitToInt x
  | otherwise = Nothing

partTwo :: [[Int]] -> Int
partTwo input = sum (map value input)

---

main :: IO ()
main = do
  inp1 <- inputDigits
  inp2 <- inputNumbers
  print $ partOne inp1
  print $ partTwo inp2
