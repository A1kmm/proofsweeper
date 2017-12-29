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
    "   " ++ map firstDigitOf [0..(gridSize - 1)] ++ "\n" ++
    "   " ++ map lastDigitOf [0..(gridSize - 1)] ++ "\n" ++
    concat [(showDigits y) ++ " " ++ renderRow y ++ "\n" |
            y <- [0..(gridSize - 1)]]
  where
    renderCoord :: Coord -> String
    renderCoord coord =
      case M.lookup coord status of
        Just (CellStatus CellKnown CellMine) -> "*"
        Just (CellStatus CellKnown CellNotMine) ->
          case M.lookup coord cnt of
            Just 0 -> " "
            Just 1 -> "1"
            Just 2 -> "2"
            Just 3 -> "3"
            Just 4 -> "4"
            Just 5 -> "5"
            Just 6 -> "6"
            Just 7 -> "7"
            _ -> "8"
        _ -> "?"
    renderRow :: Int -> String
    renderRow y =
      concat [renderCoord (Coord x y) | x <- [0..(gridSize - 1)]]
