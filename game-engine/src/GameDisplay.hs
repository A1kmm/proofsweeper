module GameDisplay (displayGame) where

import qualified Data.Map as M
import GameModel

firstDigitOf :: Int -> Char
firstDigitOf i = if i >= 10 then '1' else '0'

lastDigitOf :: Int -> Char
lastDigitOf i = case i `mod` 10 of
  0 -> '0'
  1 -> '1'
  2 -> '2'
  3 -> '3'
  4 -> '4'
  5 -> '5'
  6 -> '6'
  7 -> '7'
  8 -> '8'
  _ -> '9'

showDigits :: Int -> String
showDigits i = [firstDigitOf i, lastDigitOf i]

displayGame :: Game -> M.Map Coord Int -> String
displayGame (Game { gameGridSize = gridSize, gameStatus = status }) cnt =
    "  " ++ concatMap showDigits [0..(gridSize - 1)] ++ "\n" ++
    concat [(firstDigitOf y) : " " ++ renderRow y ++ "\n" ++
            (lastDigitOf y) : " " ++ renderRow y ++ "\n" |
            y <- [0..(gridSize - 1)]]
  where
    renderCoord :: Coord -> String
    renderCoord coord =
      case M.lookup coord status of
        Just (CellStatus CellKnown CellMine) -> "**"
        Just (CellStatus CellKnown CellNotMine) ->
          case M.lookup coord cnt of
            Just 0 -> "  "
            Just 1 -> "11"
            Just 2 -> "22"
            Just 3 -> "33"
            Just 4 -> "44"
            Just 5 -> "55"
            Just 6 -> "66"
            Just 7 -> "77"
            _ -> "88"
        _ -> "??"
    renderRow :: Int -> String
    renderRow y =
      concat [renderCoord (Coord x y) | x <- [0..(gridSize - 1)]]
